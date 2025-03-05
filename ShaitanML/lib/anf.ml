module Converter : sig
end = struct
  open Anf_ast
  open Pat_elim_ast
  open Pat_elim

  let rec convert_aexp = function
    | ImmInt i -> PEEConst (PECint i)
    | ImmBool b -> PEEConst (PECBool b)
    | ImmUnit -> PEEConst PECUnit
    | ImmVal v -> PEEVar v
    | ImmEmptyList -> PEEConst PECNil
    | ImmTuple lst -> PEETuple (List.map convert_aexp lst)
  ;;
  
  let rec convert_cexp = function
    | CEXPRAtom atom -> convert_aexp atom
    | CEXPRApp (func, args) ->
      Base.List.fold_left args ~init:(PEEVar func) ~f:(fun acc arg ->
        PEEApp (acc, convert_aexp arg))
    | CEXPRIf (cond, then_exp, else_exp) ->
      PEEIf (convert_aexp cond, convert_exp then_exp, convert_exp else_exp)
    | CEXPRCons (head, tail) ->
      PEECons (convert_aexp head, convert_aexp tail)

  and convert_exp = function
    | AELetIn (var, cexp, exp) ->
      PEELet (PENonrec (var, convert_cexp cexp), convert_exp exp)
    | AEcomplex cexp -> convert_cexp cexp
  ;;

  let convert_function (args, exp) = PEEFun (args, convert_exp exp)

  let convert_toplevel = function
    | Value (name, exp) -> PENonrec (name, convert_exp exp)
    | Non_rec (name, args, exp) -> PENonrec (name, convert_function (args, exp))
    | Rec func_list ->
      PERec
        (Base.List.map func_list ~f:(fun (name, args, exp) ->
           name, convert_function (args, exp)))
  ;;

  let to_rp_ast p = Base.List.map p ~f:convert_toplevel
end

open Anf_ast
module PrettyPrint : sig
  val pretty_print_anf_program : Format.formatter -> anf_structure -> unit
end = struct
  let name_to_string name =
    let is_valid_char = function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '#' -> true
      | _ -> false
    in
    if Base.List.for_all (Base.String.to_list name) ~f:is_valid_char
    then name
    else "( " ^ name ^ " )"
  ;;
  
  let rec atom_to_string = function
    | ImmInt i -> Int.to_string i
    | ImmBool b -> Bool.to_string b
    | ImmUnit -> "()"
    | ImmVal v -> name_to_string v
    | ImmEmptyList -> "[]"
    | ImmTuple lst ->
      let head = List.hd lst in
      let tail = List.tl lst in
      Base.List.fold
        tail
        ~init:("(" ^ atom_to_string head)
        ~f:(fun acc x -> acc ^ ", " ^ atom_to_string x)
      ^ ")"
  ;;

  let indent_string num = "\n" ^ String.make num ' '

  let rec cexp_to_string indent_level = function
    | CEXPRAtom a -> atom_to_string a
    | CEXPRApp (func, args) ->
      List.fold_left (fun acc a -> acc ^ " " ^ atom_to_string a) (name_to_string func) args
    | CEXPRIf (cond, then_exp, else_exp) ->
      let indent = indent_string indent_level in
      let if_clause = "if " ^ atom_to_string cond ^ indent in
      let then_clause = "then " ^ exp_to_string (indent_level + 2) then_exp ^ indent in
      let else_clause = "else " ^ exp_to_string (indent_level + 2) else_exp ^ indent in
      if_clause ^ then_clause ^ else_clause
    | CEXPRCons (head, tail) ->
      Format.sprintf "(%s :: %s)" (atom_to_string head) (atom_to_string tail)

  and exp_to_string indent_level = function
    | AELetIn (name, c, e) ->
      let indent = indent_string indent_level in
      Format.sprintf "let %s = %s in" name (cexp_to_string (indent_level + 2) c)
      ^ indent
      ^ exp_to_string indent_level e
    | AEcomplex e -> cexp_to_string indent_level e
  ;;

  let function_to_string (name, args, body) =
    Format.sprintf
      "%s =\n  %s"
      (Base.List.fold args ~init:name ~f:(fun acc arg -> acc ^ " " ^ arg))
      (exp_to_string 2 body)
  ;;

  let toplevel_to_string = function
    | Value (name, e) -> Format.sprintf "let %s =\n  %s" name (exp_to_string 2 e)
    | Non_rec fun1 -> "let " ^ function_to_string fun1
    | Rec func_list ->
      let first_func = List.hd func_list in
      let remaining_funcs = List.tl func_list in
      List.fold_left
        (fun acc fun1 -> acc ^ "\nand " ^ function_to_string fun1)
        (Format.sprintf "let rec " ^ function_to_string first_func)
        remaining_funcs
  ;;

  let pretty_print_anf_program ppf p =
    let length = List.length p in
    List.iteri
      (fun i a ->
        if i = length - 1
        then Format.fprintf ppf "%s" (toplevel_to_string a)
        else Format.fprintf ppf "%s\n" (toplevel_to_string a))
      p
  ;;

  let pretty_print_error ppf = function
    | IncorrectAst s -> Format.fprintf ppf "Got incorrect ast: %s" s
  ;;
end

open Pat_elim_ast
open Pat_elim
open Common.MonadCounterError
open Base

let empty_map = Map.empty (module String)

let merge_maps last new1 =
  Map.merge_skewed last new1 ~combine:(fun ~key:_ _ v2 -> v2)
;;

let create_name i = "a" ^ Int.to_string i

let const_to_aexp = function
  | PECint i -> ImmInt i
  | PECBool b -> ImmBool b
  | PECUnit -> ImmUnit
  | PECNil -> ImmEmptyList
;;

let rec to_aexp e =
  match e with
  | PEEVar v -> return ([], ImmVal v)
  | PEEConst c -> return ([], const_to_aexp c)
  | e ->
    let* fresh = fresh >>| create_name in
    let* binds1, e = to_cexp e in
    return (binds1 @ [ fresh, e ], ImmVal fresh)

and to_cexp = function
  | PEEConst c -> return ([], cexp_atom @@ const_to_aexp c)
  | PEEVar v -> return ([], cexp_atom @@ ImmVal v)
  | PEEApp (e1, e2) -> app_to_cexp e1 e2
  | PEELet (PENonrec (name, e1), e2) ->
    let* binds1, e1 = to_cexp e1 in
    let* binds2, e2 = to_cexp e2 in
    return (binds1 @ [ name, e1 ] @ binds2, e2)
  | PEEIf (e1, e2, e3) ->
    let* binds, e1 = to_aexp e1 in
    let* e2 = to_exp e2 in
    let* e3 = to_exp e3 in
    return (binds, cexp_ite e1 e2 e3)
  | PEELet (PERec _, _) ->
    fail @@ IncorrectAst "Ast contains recursive let-in declarations"
  | PEEFun _ -> fail @@ IncorrectAst "Ast contains no toplevel function"
  | PEETuple e_list ->
    let* binds, e_list = List.fold_right e_list ~init:(return ([], [])) ~f:(fun e acc ->
      let* acc_binds, acc_list = acc in
      let* binds, e = to_aexp e in
      return (binds @ acc_binds, e :: acc_list)) in
    return (binds, cexp_atom @@ ImmTuple e_list)
  | PEECons (e1, e2) ->
    let* binds1, e1 = to_aexp e1 in
    let* binds2, e2 = to_aexp e2 in
    return (binds1 @ binds2, cexp_cons_list e1 e2)

and app_to_cexp e1 e2 =
  let rec helper = function
    | PEEApp (e1, e2) ->
      let f, args_e = helper e1 in
      f, e2 :: args_e
    | e -> e, []
  in
  let to_app, args_e = helper @@ PEEApp (e1, e2) in
  let args_e = List.rev args_e in
  let rec process_args acc = function
    | [] -> return acc
    | expr :: rest ->
      let cur_exprs, cur_binds = acc in
      match expr with
      | PEEVar v -> process_args (ImmVal v :: cur_exprs, cur_binds) rest
      | PEEConst c -> process_args (const_to_aexp c :: cur_exprs, cur_binds) rest
      | _ ->
        let* fresh = fresh >>| create_name in
        let* new_binds, f_cexp = to_cexp expr in
        process_args (ImmVal fresh :: cur_exprs, cur_binds @ new_binds @ [ fresh, f_cexp ] @ cur_binds) rest
  in
  let* exprs, binds = process_args ([], []) (to_app :: args_e) in
  let exprs = List.rev exprs in
  let to_app, args_e = List.hd_exn exprs, List.tl_exn exprs in
  match to_app with
  | ImmVal to_app -> return (binds, cexp_app to_app args_e)
  | _ -> fail @@ IncorrectAst "Ast contains wrong-typed application"

and to_exp e =
    let* binds, init = to_cexp e in
    let rec fold_binds = function
      | [] -> return @@ AEcomplex init
      | (name, cexp) :: rest ->
        let* rest_result = fold_binds rest in
        return @@ aexpr_let_in name cexp rest_result
    in
    fold_binds binds

let anf_toplevel = function
  | PENonrec (name, e) ->
    (match e with
     | PEEFun (args, body) ->
       let* new_body = to_exp body in
       return @@ [ Non_rec (name, args, new_body) ]
     | _ ->
       let* new_e = to_exp e in
       return @@ [ Value (name, new_e) ])
  | PERec decls ->
    let vals =
      List.filter decls ~f:(fun (_, e) ->
        match e with
        | PEEFun _ -> false
        | _ -> true)
    in
    let funcs =
      List.filter_map decls ~f:(fun (name, e) ->
        match e with
        | PEEFun (args, body) -> Some (name, args, body)
        | _ -> None)
    in
    let rec process_vals = function
      | [] -> return []
      | (name, e) :: rest ->
        let* new_e = to_exp e in
        let* rest_result = process_vals rest in
        return @@ Value (name, new_e) :: rest_result
    in
    let rec process_funcs = function
      | [] -> return []
      | (name, args, body) :: rest ->
        let* new_body = to_exp body in
        let* rest_result = process_funcs rest in
        return @@ (name, args, new_body) :: rest_result
    in
    let* vals = process_vals vals in
    let* funcs = process_funcs funcs in
    return @@ vals @ [ Rec funcs ]
;;

let anf_program program =
  let rec helper = function
    | [] -> return []
    | hd :: tl ->
      let* d1 = anf_toplevel hd in
      let* d2 = helper tl in
      return @@ d1 @ d2
  in
  helper program
;;

let convert_to_anf nh init_num p =
  match run (anf_program p) nh init_num with
  | _, _, r -> r
;;

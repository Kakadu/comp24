(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(* Inspired by https://matt.might.net/articles/a-normalization/ *)

(** atomic expressions *)
type aexp =
  | Ae_int of int
  | Ae_bool of bool
  | Ae_unit
  | Ae_val of string
  | Ae_empty_list
  | Ae_tuple of aexp list

let ae_int i = Ae_int i
let ae_bool b = Ae_bool b
let ae_val v = Ae_val v
let ae_tuple lst = Ae_tuple lst

(** compex expressions *)
type cexp =
  | Ce_atom of aexp
  | Ce_app of aexp * aexp list
  | Ce_ite of aexp * exp * exp
  | Ce_cons_list of aexp * aexp

and exp =
  | E_let_in of string * cexp * exp
  | E_complex of cexp

let ce_atom aexp = Ce_atom aexp
let ce_app e e_list = Ce_app (e, e_list)
let ce_ite i t e = Ce_ite (i, t, e)
let ce_cons_list aexp1 aexp2 = Ce_cons_list (aexp1, aexp2)
let e_let_in name cexp exp = E_let_in (name, cexp, exp)
let e_complex cexp = E_complex cexp

type func = string * string list * exp

type toplevel =
  | Value of string * exp
  | Non_rec of func
  | Rec of func list

type anf_program = toplevel list
type error = IncorrectAst of string

module Convert : sig
  val to_ast : anf_program -> Ast.program
  val to_rp_ast : anf_program -> Remove_patterns.rp_program
end = struct
  open Remove_patterns

  let rec convert_aexp = function
    | Ae_int i -> Rp_e_const (Rp_c_int i)
    | Ae_bool b -> Rp_e_const (Rp_c_bool b)
    | Ae_unit -> Rp_e_const Rp_c_unit
    | Ae_val v -> Rp_e_ident v
    | Ae_empty_list -> Rp_e_const Rp_c_empty_list
    | Ae_tuple lst -> Rp_e_tuple (List.map convert_aexp lst)
  ;;

  let rec convert_cexp = function
    | Ce_atom atom -> convert_aexp atom
    | Ce_app (to_app, args) ->
      Base.List.fold_left args ~init:(convert_aexp to_app) ~f:(fun acc arg ->
        Rp_e_app (acc, convert_aexp arg))
    | Ce_ite (aexp, cexp1, cexp2) ->
      Rp_e_ite (convert_aexp aexp, convert_exp cexp1, convert_exp cexp2)
    | Ce_cons_list (aexp1, aexp2) ->
      Rp_e_cons_list (convert_aexp aexp1, convert_aexp aexp2)

  and convert_exp = function
    | E_let_in (name, cexp, exp) ->
      Rp_e_let (Rp_non_rec (name, convert_cexp cexp), convert_exp exp)
    | E_complex cexp -> convert_cexp cexp
  ;;

  let convert_fun (args, exp) = Rp_e_fun (args, convert_exp exp)

  let convert_toplevel = function
    | Value (name, exp) -> Rp_non_rec (name, convert_exp exp)
    | Non_rec (name, args, exp) -> Rp_non_rec (name, convert_fun (args, exp))
    | Rec func_list ->
      Rp_rec
        (Base.List.map func_list ~f:(fun (name, args, exp) ->
           name, convert_fun (args, exp)))
  ;;

  let to_rp_ast p = Base.List.map p ~f:convert_toplevel
  let to_ast p = to_rp_ast p |> Remove_patterns.ToAst.convert_program
end

let f a b c =
  let sum a b = a + b in
  let inc x =
    let p t = t + 1 in
    sum x (p 1)
  in
  if a > 0 then sum b c else inc c
;;

module PP : sig
  val pp_anf_program : Format.formatter -> anf_program -> unit
  val pp_error : Format.formatter -> error -> unit
end = struct
  let rec atom_to_str = function
    | Ae_int i -> Int.to_string i
    | Ae_bool b -> Bool.to_string b
    | Ae_unit -> "()"
    | Ae_val v ->
      let is_valname = function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '#' -> true
        | _ -> false
      in
      if Base.List.for_all (Base.String.to_list v) ~f:is_valname
      then v
      else "( " ^ v ^ " )"
    | Ae_empty_list -> "[]"
    | Ae_tuple lst ->
      let hd = List.hd lst in
      let tl = List.tl lst in
      Base.List.fold
        tl
        ~init:("(" ^ atom_to_str hd)
        ~f:(fun acc x -> acc ^ ", " ^ atom_to_str x)
      ^ ")"
  ;;

  let create_tab num = "\n" ^ String.make num ' '

  let rec cexp_to_str tab_num = function
    | Ce_atom a -> atom_to_str a
    | Ce_app (a1, a_list) ->
      List.fold_left (fun acc a -> acc ^ " " ^ atom_to_str a) (atom_to_str a1) a_list
    | Ce_ite (a, e1, e2) ->
      let tab = create_tab tab_num in
      let if1 = "if " ^ atom_to_str a ^ tab in
      let then1 = "then " ^ exp_to_str (tab_num + 2) e1 ^ tab in
      let else1 = "else " ^ exp_to_str (tab_num + 2) e2 ^ tab in
      if1 ^ then1 ^ else1
    | Ce_cons_list (a1, a2) ->
      Format.sprintf "(%s :: %s)" (atom_to_str a1) (atom_to_str a2)

  and exp_to_str tab_num = function
    | E_let_in (name, c, e) ->
      let tab = create_tab tab_num in
      Format.sprintf "let %s = %s in" name (cexp_to_str (tab_num + 2) c)
      ^ tab
      ^ exp_to_str tab_num e
    | E_complex e -> cexp_to_str tab_num e
  ;;

  let fun_to_str (name, args, body) =
    Format.sprintf
      "%s =\n  %s"
      (Base.List.fold args ~init:name ~f:(fun acc arg -> acc ^ " " ^ arg))
      (exp_to_str 2 body)
  ;;

  let toplevel_to_str = function
    | Value (name, e) -> Format.sprintf "let %s =\n  %s" name (exp_to_str 2 e)
    | Non_rec fun1 -> "let " ^ fun_to_str fun1
    | Rec func_list ->
      let fun1 = List.hd func_list in
      let tl = List.tl func_list in
      List.fold_left
        (fun acc fun1 -> acc ^ "\nand " ^ fun_to_str fun1)
        (Format.sprintf "let rec " ^ fun_to_str fun1)
        tl
  ;;

  let pp_anf_program ppf p =
    let len = List.length p in
    List.iteri
      (fun i a ->
        if i = len - 1
        then Format.fprintf ppf "%s" (toplevel_to_str a)
        else Format.fprintf ppf "%s\n" (toplevel_to_str a))
      p
  ;;

  let pp_error ppf e =
    match e with
    | IncorrectAst s -> Format.fprintf ppf "Got incorrect ast: %s" s
  ;;
end

open Remove_patterns
open Common.MonadCounterError
open Base

let empty_bindings = Map.empty (module String)

let update_bindings last new1 =
  Map.merge_skewed last new1 ~combine:(fun ~key:_ _ v2 -> v2)
;;

let get_name i = "#" ^ Int.to_string i

let const_to_aexp c =
  match c with
  | Rp_c_int i -> ae_int i
  | Rp_c_bool b -> ae_bool b
  | Rp_c_unit -> Ae_unit
  | Rp_c_empty_list -> Ae_empty_list
;;

let rec to_aexp e =
  match e with
  | Rp_e_ident v -> return ([], ae_val v)
  | Rp_e_const c -> return ([], const_to_aexp c)
  | _ ->
    let* fresh = fresh >>| get_name in
    let* binds1, e = to_cexp e in
    return (binds1 @ [ fresh, e ], ae_val fresh)

and to_cexp = function
  | Rp_e_const c -> return ([], ce_atom @@ const_to_aexp c)
  | Rp_e_ident v -> return ([], ce_atom @@ ae_val v)
  | Rp_e_app (e1, e2) -> app_to_cexp e1 e2
  | Rp_e_let (Rp_non_rec (name, e1), e2) ->
    let* binds1, e1 = to_cexp e1 in
    let* binds2, e2 = to_cexp e2 in
    return (binds1 @ [ name, e1 ] @ binds2, e2)
  | Rp_e_ite (e1, e2, e3) ->
    let* binds, e1 = to_aexp e1 in
    let* e2 = to_exp e2 in
    let* e3 = to_exp e3 in
    return (binds, ce_ite e1 e2 e3)
  | Rp_e_let (Rp_rec _, _) ->
    fail @@ IncorrectAst "Ast contains recursive let-in declarations"
  | Rp_e_fun _ -> fail @@ IncorrectAst "Ast contains no toplevel function"
  | Rp_e_tuple e_list ->
    let* binds, e_list = RList.map e_list ~f:to_aexp >>| List.unzip in
    return (List.concat binds, ce_atom @@ ae_tuple e_list)
  | Rp_e_cons_list (e1, e2) ->
    let* binds1, e1 = to_aexp e1 in
    let* binds2, e2 = to_aexp e2 in
    return (binds1 @ binds2, ce_cons_list e1 e2)

and app_to_cexp e1 e2 =
  let rec helper = function
    | Rp_e_app (e1, e2) ->
      let f, args_e = helper e1 in
      f, e2 :: args_e
    | e -> e, []
  in
  let to_app, args_e = helper @@ Rp_e_app (e1, e2) in
  let args_e = List.rev args_e in
  let f1 acc expr =
    let cur_exprs, cur_binds = acc in
    match expr with
    | Rp_e_ident v -> return (ae_val v :: cur_exprs, cur_binds)
    | Rp_e_const c -> return (const_to_aexp c :: cur_exprs, cur_binds)
    | _ ->
      let* fresh = fresh >>| get_name in
      let* new_binds, f_cexp = to_cexp expr in
      return (ae_val fresh :: cur_exprs, cur_binds @ new_binds @ [ fresh, f_cexp ])
  in
  let* exprs, binds = RList.fold_left (to_app :: args_e) ~init:(return ([], [])) ~f:f1 in
  let exprs = List.rev exprs in
  let to_app, args_e = List.hd_exn exprs, List.tl_exn exprs in
  return (binds, ce_app to_app args_e)

and to_exp = function
  | _ as orig ->
    let* binds, init = to_cexp orig in
    RList.fold_right
      binds
      ~init:(return @@ e_complex init)
      ~f:(fun (name, cexp) acc -> return @@ e_let_in name cexp acc)
;;

let anf_toplevel = function
  | Rp_non_rec (name, e) ->
    (match e with
     | Rp_e_fun (args, body) ->
       let* new_body = to_exp body in
       return @@ [ Non_rec (name, args, new_body) ]
     | _ ->
       let* new_e = to_exp e in
       return @@ [ Value (name, new_e) ])
  | Rp_rec decls ->
    let vals =
      List.filter decls ~f:(fun (_, e) ->
        match e with
        | Rp_e_fun _ -> false
        | _ -> true)
    in
    let funcs =
      List.filter_map decls ~f:(fun (name, e) ->
        match e with
        | Rp_e_fun (args, body) -> Some (name, args, body)
        | _ -> None)
    in
    let* vals =
      RList.map vals ~f:(fun (name, e) ->
        let* new_e = to_exp e in
        return @@ Value (name, new_e))
    in
    let* funcs =
      RList.map funcs ~f:(fun (name, args, body) ->
        let* new_body = to_exp body in
        return @@ (name, args, new_body))
    in
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

let run_to_anf_program init_num p = run init_num (anf_program p)

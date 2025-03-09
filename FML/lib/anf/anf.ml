open Common
open Anf_ast
open Pe_ast
open Base
open Common.MonadCounter

let cexp_app e e_list = CEApply (e, e_list)
let cexp_ite i t e = CEIf (i, t, e)
let cexp_cons aexp1 aexp2 = CECons (aexp1, aexp2)
let aexpr_let_in name cexp exp = ALetIn (name, cexp, exp)
let aexpr_complex cexp = ACExpr cexp

let imm_int int_ = ImmInt int_
let imm_bool bool_ = ImmBool bool_
let imm_var var_ = ImmIdentifier var_
let imm_tuple lst_ = ImmTuple lst_
let const_to_immexp = function
  | Pe_Cint i -> ImmInt i
  | Pe_CBool b -> ImmBool b
;;


let rec to_immexp = function
  | Pe_EIdentifier v -> return ([], imm_var v)
  | Pe_EConst c -> return ([], const_to_immexp c)
  | Pe_EUnit -> return ([], ImmUnit)
  | Pe_ENill -> return ([], ImmNill)
  | e ->
    let* fresh = fresh >>| get_id in
    let* binds1, e = to_cexp e in
    return (binds1 @ [ fresh, e ], imm_var fresh)

and to_cexp = function
  | Pe_EUnit -> return ([], cimmexpr @@ ImmUnit)
  | Pe_ENill -> return ([], cimmexpr @@ ImmNill)
  | Pe_EConst c -> return ([], cimmexpr @@ const_to_immexp c)
  | Pe_EIdentifier v -> return ([], cimmexpr @@ imm_var v)
  | Pe_EApp (e1, e2) -> app_to_cexp e1 e2
  | Pe_ELet (NoRec, name, e1, e2) ->
    let* binds1, e1 = to_cexp e1 in
    let* binds2, e2 = to_cexp e2 in
    return (binds1 @ [ name, e1 ] @ binds2, e2)
  | Pe_EIf (e1, e2, e3) ->
    let* binds, e1 = to_immexp e1 in
    let* e2 = to_exp e2 in
    let* e3 = to_exp e3 in
    return (binds, cexp_ite e1 e2 e3)
  | Pe_ETuple e_list ->
    let* binds, e_list = map e_list ~f:to_immexp >>| List.unzip in
    return (List.concat binds, cimmexpr @@ imm_tuple e_list)
  | Pe_ECons (e1, e2) ->
    let* binds1, e1 = to_immexp e1 in
    let* binds2, e2 = to_immexp e2 in
    return (binds1 @ binds2, cexp_cons e1 e2)
  | _ -> return ([], cimmexpr @@ ImmUnit)

and app_to_cexp e1 e2 =
  let rec helper = function
    | Pe_EApp (e1, e2) ->
      let f, args_e = helper e1 in
      f, e2 :: args_e
    | e -> e, []
  in
  let to_app, args_e = helper @@ Pe_EApp (e1, e2) in
  let args_e = List.rev args_e in
  let f1 acc expr =
    let cur_exprs, cur_binds = acc in
    match expr with
    | Pe_EIdentifier v -> return (imm_var v :: cur_exprs, cur_binds)
    | Pe_EConst c -> return (const_to_immexp c :: cur_exprs, cur_binds)
    | _ ->
      let* fresh = fresh >>| get_id in
      let* new_binds, f_cexp = to_cexp expr in
      return (imm_var fresh :: cur_exprs, new_binds @ [ fresh, f_cexp ] @ cur_binds)
  in
  let* exprs, binds = fold_left (to_app :: args_e) ~init:(return ([], [])) ~f:f1 in
  let exprs = List.rev exprs in
  match List.hd_exn exprs with
  | ImmIdentifier to_app -> 
    let args_e = List.tl_exn exprs in
    return (binds, cexp_app to_app args_e)
  | _ -> failwith "Unexpected expression in application"

and to_exp e =
  let* binds, init = to_cexp e in
  fold_right
    binds
    ~init:(return @@ aexpr_complex init)
    ~f:(fun (name, cexp) acc -> return @@ aexpr_let_in name cexp acc)
;;

let anf_str_item = function
  | Pe_Nonrec decls ->
    let* bindings =
      map decls ~f:(fun (name, e) ->
        match e with
        | Pe_EFun (args, body) ->
          let* new_body = to_exp body in
          return (ADNoRec (ALet (name, args, new_body)))
        | _ ->
          let* new_e = to_exp e in
          return (ADNoRec (ALet (name, [], new_e))))
    in
    return bindings
  | Pe_Rec decls ->
    let* bindings =
      map decls ~f:(fun (name, e) ->
        match e with
        | Pe_EFun (args, body) ->
          let* new_body = to_exp body in
          return (ALet (name, args, new_body))
        | _ ->
          let* new_e = to_exp e in
          return (ALet (name, [], new_e)))
    in
    return [ ADREC bindings ]
;;

let anf_structure structure =
  let rec helper = function
    | [] -> return []
    | hd :: tl ->
      let* d1 = anf_str_item hd in
      let* d2 = helper tl in
      return @@ d1 @ d2
  in
  helper structure
;;

let run_anf bindings init structure = run (anf_structure structure) bindings init
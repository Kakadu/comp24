open Common
open Anf_ast
open Pat_elim_ast
open Base
open Common.MonadCounter
open Pat_elim

let rec to_aexp = function
  | PEEVar v -> return ([], imm_var v)
  | PEEConst c -> return ([], const_to_aexp c)
  | e ->
    let* fresh = fresh >>| get_id in
    let* binds1, e = to_cexp e in
    return (binds1 @ [ fresh, e ], imm_var fresh)

and to_cexp = function
  | PEEConst c -> return ([], cexp_atom @@ const_to_aexp c)
  | PEEVar v -> return ([], cexp_atom @@ imm_var v)
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
  | PEETuple e_list ->
    let* binds, e_list = map e_list ~f:to_aexp >>| List.unzip in
    return (List.concat binds, cexp_atom @@ imm_tuple e_list)
  | PEECons (e1, e2) ->
    let* binds1, e1 = to_aexp e1 in
    let* binds2, e2 = to_aexp e2 in
    return (binds1 @ binds2, cexp_cons e1 e2)

and app_to_cexp e1 e2 =
  let rec helper = function
    | PEEApp (e1, e2) ->
      let f, args_e = helper e1 in
      f, e2 :: args_e
    | e -> e, []
  in
  let to_app, args_e = helper @@ PEEApp (e1, e2) in
  let args_e = List.rev args_e in
  let f1 acc expr =
    let cur_exprs, cur_binds = acc in
    match expr with
    | PEEVar v -> return (imm_var v :: cur_exprs, cur_binds)
    | PEEConst c -> return (const_to_aexp c :: cur_exprs, cur_binds)
    | _ ->
      let* fresh = fresh >>| get_id in
      let* new_binds, f_cexp = to_cexp expr in
      return (imm_var fresh :: cur_exprs, new_binds @ [ fresh, f_cexp ] @ cur_binds)
  in
  let* exprs, binds = fold_left (to_app :: args_e) ~init:(return ([], [])) ~f:f1 in
  let exprs = List.rev exprs in
  let ImmVar to_app, args_e = List.hd_exn exprs, List.tl_exn exprs in
  return (binds, cexp_app to_app args_e)

and to_exp e =
  let* binds, init = to_cexp e in
  fold_right
    binds
    ~init:(return @@ aexpr_complex init)
    ~f:(fun (name, cexp) acc -> return @@ aexpr_let_in name cexp acc)
;;

let anf_str_item = function
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
    let* vals =
      map vals ~f:(fun (name, e) ->
        let* new_e = to_exp e in
        return @@ Value (name, new_e))
    in
    let* funcs =
      map funcs ~f:(fun (name, args, body) ->
        let* new_body = to_exp body in
        return @@ (name, args, new_body))
    in
    return @@ vals @ [ Rec funcs ]
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

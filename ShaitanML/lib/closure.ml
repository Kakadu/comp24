open Pat_elim_ast
open Common
open Base


let rec free_vars env =
  let open StrSet in
  function
  | PEEConst _ -> empty
  | PEEVar id -> if find env id then empty else singleton id
  | PEEIf (e1, e2, e3) -> union_list [ free_vars env e1; free_vars env e2; free_vars env e3 ]
  | PEEFun (args, body) ->
    let binded = union env (of_list args) in
    free_vars binded body
  | PEEApp (e1, e2) -> union (free_vars env e1) (free_vars env e2)
  | PEELet (PENonrec (name, e1), e2) ->
    union (free_vars env e1) (free_vars (add env name) e2)
  | PEELet (PERec cl, e) ->
    let ids, el =
      List.fold_right cl ~init:(empty, []) ~f:(fun (name, e) (ids, exprs) ->
        add ids name, e :: exprs)
    in
    let binded = union env ids in
    List.fold el ~init:(free_vars binded e) ~f:(fun acc e -> union acc (free_vars binded e))
  | PEECons (e1, e2) -> union (free_vars env e1) (free_vars env e2)
  | PEETuple el -> List.fold el ~init:empty ~f:(fun acc e -> union acc (free_vars env e))
;;

let make_apply expr args env =
  List.fold args ~init:expr ~f:(fun acc name ->
    let arg =
      match StrMap.find env name with
      | Some e -> e
      | None -> PEEVar name
    in
    PEEApp (acc, arg))
;;

let rec cc_expr global_env local_env = function
  | PEEConst _ as c -> c
  | PEEVar id as v ->
    (match StrMap.find local_env id with
     | Some new_expr -> new_expr
     | None -> v)
  | PEEIf (e1, e2, e3) ->
    let e1 = cc_expr global_env local_env e1 in
    let e2 = cc_expr global_env local_env e2 in
    let e3 = cc_expr global_env local_env e3 in
    PEEIf (e1, e2, e3)
  | PEEFun (args, body) as v ->
    let fvs = free_vars global_env v |> StrSet.to_list in
    let body = cc_expr global_env empty body in
    let e = PEEFun (fvs @ args, body) in
    make_apply e fvs local_env
  | PEEApp (e1, e2) ->
    let e1 = cc_expr global_env local_env e1 in
    let e2 = cc_expr global_env local_env e2 in
    PEEApp (e1, e2)
  | PEELet (PENonrec (name, e1), e2) ->
    let e1, env1 = cc_nonrec global_env local_env name e1 in
    let env2 = StrMap.merge_two local_env env1 in
    let e2 = cc_expr global_env env2 e2 in
    PEELet (PENonrec (name, e1), e2)
  | PEELet (PERec decl_list, e2) ->
    let cl, env1 = cc_rec global_env local_env decl_list in
    let env2 = StrMap.merge_two local_env env1 in
    let e2 = cc_expr global_env env2 e2 in
    PEELet (PERec cl, e2)
  | PEECons (e1, e2) ->
    let e1 = cc_expr global_env local_env e1 in
    let e2 = cc_expr global_env local_env e2 in
    PEECons (e1, e2)
  | PEETuple el ->
    let el = List.map el ~f:(cc_expr global_env local_env) in
    PEETuple el

and cc_nonrec global_env local_env name = function
  | PEEFun (args, body) ->
    let fvs = StrSet.(to_list (diff (free_vars global_env body) (of_list args))) in
    let body = cc_expr global_env empty body in
    let e = PEEFun (fvs @ args, body) in
    let apply = make_apply (PEEVar name) fvs local_env in
    e, StrMap.singleton name apply
  | expr -> cc_expr global_env local_env expr, empty

and cc_rec global_env prev_env cl =
  let ids = List.map cl ~f:fst in
  let f1 (free, env) (name, expr) =
    match expr with
    | PEEFun (args, body) ->
      let remove = StrSet.union (StrSet.of_list ids) (StrSet.of_list args) in
      let fvs = StrSet.diff (free_vars global_env body) remove |> StrSet.to_list in
      let bind = make_apply (PEEVar name) fvs prev_env in
      let env = StrMap.update env name ~f:(fun _ -> bind) in
      fvs :: free, env
    | _ -> [] :: free, env
  in
  let fvs, env = List.fold cl ~init:([], prev_env) ~f:f1 in
  let fvs = List.rev fvs in
  let to_fold = List.zip_exn cl fvs in
  let f1 decl_acc ((name, e), free) =
    match e with
    | PEEFun (args, body) ->
      let new_body = cc_expr global_env empty body in
      let efun = PEEFun (free @ args, new_body) in
      (name, efun) :: decl_acc
    | _ ->
      let e = cc_expr global_env env e in
      (name, e) :: decl_acc
  in
  let cl = List.fold to_fold ~init:[] ~f:f1 in
  let cl = List.rev cl in
  cl, env
;;

let cc_str_item global_env = function
  | PENonrec (name, e) ->
    let e1, _ = cc_nonrec global_env empty name e in
    let env = StrSet.add global_env name in
    env, PENonrec (name, e1)
  | PERec decl_list ->
    let ids = List.map decl_list ~f:fst in
    let cl, _ = cc_rec global_env empty decl_list in
    let env =
      List.fold ids ~init:global_env ~f:(fun acc name -> StrSet.add acc name)
    in
    env, PERec cl
;;

let run_cc structure =
  let builtins = List.fold Common.builtins ~init:StrSet.empty ~f:StrSet.add in
  let rec helper last_env = function
    | [] -> []
    | hd :: tl ->
      let env, ast = cc_str_item last_env hd in
      ast :: helper env tl
  in
  helper builtins structure
;;

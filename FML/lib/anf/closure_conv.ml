(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Pe_ast
open Common
open Base

let rec free_vars binded =
  let open StrSet in
  function
  | Pe_EConst _ | Pe_ENill | Pe_EUnit -> StrSet.empty
  | Pe_EIdentifier id -> if find binded id then empty else singleton id
  | Pe_EIf (e1, e2, e3) ->
    union_list [ free_vars binded e1; free_vars binded e2; free_vars binded e3 ]
  | Pe_EFun (args, body) ->
    let binded = union binded (of_list args) in
    free_vars binded body
  | Pe_EApp (e1, e2) -> union (free_vars binded e1) (free_vars binded e2)
  | Pe_ELet (NoRec, name, e1, e2) ->
    union (free_vars binded e1) (free_vars (add binded name) e2)
  | Pe_ELet (Rec, name, e1, e2) ->
    let binded = add binded name in
    union (free_vars binded e1) (free_vars binded e2)
    (* hmmmmm....*)
  | Pe_ECons (e1, e2) -> union (free_vars binded e1) (free_vars binded e2)
  | Pe_ETuple es ->
    List.fold es ~init:empty ~f:(fun acc e -> union acc (free_vars binded e))
;;

let make_apply expr args env =
  List.fold args ~init:expr ~f:(fun acc name ->
    let arg =
      match StrMap.find env name with
      | Some e -> e
      | None -> Pe_EIdentifier name
    in
    Pe_EApp (acc, arg))
;;

let rec cc_expr global_env bindings = function
  | Pe_EIdentifier id as v ->
    (match StrMap.find bindings id with
     | Some new_expr -> new_expr
     | None -> v)
  | Pe_EIf (e1, e2, e3) ->
    let e1 = cc_expr global_env bindings e1 in
    let e2 = cc_expr global_env bindings e2 in
    let e3 = cc_expr global_env bindings e3 in
    Pe_EIf (e1, e2, e3)
  | Pe_EFun (args, body) as v ->
    let fvs = free_vars global_env v |> StrSet.to_list in
    let body = cc_expr global_env empty body in
    let e = Pe_EFun (fvs @ args, body) in
    make_apply e fvs bindings
  | Pe_EApp (e1, e2) ->
    let e1 = cc_expr global_env bindings e1 in
    let e2 = cc_expr global_env bindings e2 in
    Pe_EApp (e1, e2)
  | Pe_ELet (NoRec, name, e1, e2) ->
    let e1, env1 =
      match e1 with
      | Pe_EFun (args, body) ->
        let fvs = StrSet.(to_list (diff (free_vars global_env body) (of_list args))) in
        let body = cc_expr global_env empty body in
        let e = Pe_EFun (fvs @ args, body) in
        let apply = make_apply (Pe_EIdentifier name) fvs bindings in
        e, StrMap.singleton name apply
      | expr -> cc_expr global_env bindings expr, empty
    in
    let env2 = StrMap.merge_two bindings env1 in
    let e2 = cc_expr global_env env2 e2 in
    Pe_ELet (NoRec, name, e1, e2)
  | Pe_ELet (Rec, name, e1, e2) ->
    let e1, env1 =
      match e1 with
      | Pe_EFun (args, body) ->
        let fvs =
          StrSet.(to_list (diff (free_vars global_env body) (of_list (name :: args))))
        in
        let apply = make_apply (Pe_EIdentifier name) fvs bindings in
        let body = cc_expr global_env (StrMap.singleton name apply) body in
        let e = Pe_EFun (fvs @ args, body) in
        let apply = make_apply (Pe_EIdentifier name) fvs bindings in
        e, StrMap.singleton name apply
      | expr -> cc_expr global_env bindings expr, empty
    in
    let env2 = StrMap.merge_two bindings env1 in
    let e2 = cc_expr global_env env2 e2 in
    Pe_ELet (Rec, name, e1, e2)
  | Pe_ECons (e1, e2) ->
    let e1 = cc_expr global_env bindings e1 in
    let e2 = cc_expr global_env bindings e2 in
    Pe_ECons (e1, e2)
  | Pe_ETuple el ->
    let el = List.map el ~f:(cc_expr global_env bindings) in
    Pe_ETuple el
  | c -> c
;;

let cc_nonrec global_env decl_list =
  let f1 (decl_acc, env) (name, expr) =
    match expr with
    | Pe_EFun (args, body) ->
      let fvs = StrSet.(to_list (diff (free_vars global_env body) (of_list args))) in
      let body = cc_expr global_env empty body in
      let e = Pe_EFun (fvs @ args, body) in
      (name, e) :: decl_acc, StrSet.add env name
    | expr -> (name, cc_expr global_env empty expr) :: decl_acc, StrSet.add env name
  in
  List.fold decl_list ~init:([], global_env) ~f:f1
;;

let cc_rec global_env prev_env cl =
  let ids = List.map cl ~f:fst in
  let f1 (free, env) (name, expr) =
    match expr with
    | Pe_EFun (args, body) ->
      let remove = StrSet.union (StrSet.of_list ids) (StrSet.of_list args) in
      let fvs = StrSet.diff (free_vars global_env body) remove |> StrSet.to_list in
      let bind = make_apply (Pe_EIdentifier name) fvs prev_env in
      let env = StrMap.update env name ~f:(fun _ -> bind) in
      fvs :: free, env
    | _ -> [] :: free, env
  in
  let fvs, env = List.fold cl ~init:([], prev_env) ~f:f1 in
  let fvs = List.rev fvs in
  let to_fold = List.zip_exn cl fvs in
  let f1 decl_acc ((name, e), free) =
    match e with
    | Pe_EFun (args, body) ->
      let new_body = cc_expr global_env empty body in
      let efun = Pe_EFun (free @ args, new_body) in
      (name, efun) :: decl_acc
    | _ ->
      let e = cc_expr global_env env e in
      (name, e) :: decl_acc
  in
  let cl = List.fold to_fold ~init:[] ~f:f1 in
  let cl = List.rev cl in
  cl, env
;;

let cc_declaration global_env = function
  | Pe_Nonrec decl_list ->
    let decl_list, env = cc_nonrec global_env decl_list in
    env, Pe_Nonrec decl_list
  | Pe_Rec decl_list ->
    let ids = List.map decl_list ~f:fst in
    let cl, _ = cc_rec global_env empty decl_list in
    let env = List.fold ids ~init:global_env ~f:(fun acc name -> StrSet.add acc name) in
    env, Pe_Rec cl
;;

let run_cc ast =
  let builtins = List.fold Common.builtins ~init:StrSet.empty ~f:StrSet.add in
  let rec helper last_env = function
    | [] -> []
    | hd :: tl ->
      let env, ast = cc_declaration last_env hd in
      ast :: helper env tl
  in
  helper builtins ast
;;

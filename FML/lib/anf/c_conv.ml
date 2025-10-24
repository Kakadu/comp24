(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Me_ast
open Common

let rec expr_free_vars binded =
  let open StrSet in
  function
  | Me_EConst _ | Me_ENill | Me_EUnit -> StrSet.empty
  | Me_EIdentifier id when find binded id -> empty
  | Me_EIdentifier id -> singleton id
  | Me_EIf (e1, e2, e3) ->
    union_list
      [ expr_free_vars binded e1; expr_free_vars binded e2; expr_free_vars binded e3 ]
  | Me_ECons (e1, e2) | Me_EApp (e1, e2) ->
    union (expr_free_vars binded e1) (expr_free_vars binded e2)
  | Me_ETuple es ->
    List.fold_left (fun acc expr -> union acc (expr_free_vars binded expr)) empty es
  | Me_EFun (args, expr) ->
    let binded = union binded (of_list args) in
    expr_free_vars binded expr
  | Me_ELet (NoRec, name, e1, e2) ->
    let binded' = add binded name in
    union (expr_free_vars binded e1) (expr_free_vars binded' e2)
  | Me_ELet (Rec, name, e1, e2) ->
    let binded = add binded name in
    union (expr_free_vars binded e1) (expr_free_vars binded e2)
;;

let rec cc_expr env bindings = function
  | Me_EIdentifier id ->
    (match StrMap.find bindings id with
     | Some new_expr -> new_expr
     | None -> Me_EIdentifier id)
  | Me_ETuple es -> Me_ETuple (List.map (fun expr -> cc_expr env bindings expr) es)
  | Me_EIf (e1, e2, e3) ->
    Me_EIf (cc_expr env bindings e1, cc_expr env bindings e2, cc_expr env bindings e3)
  | Me_ECons (e1, e2) -> Me_ECons (cc_expr env bindings e1, cc_expr env bindings e2)
  | Me_EApp (e1, e2) -> Me_EApp (cc_expr env bindings e1, cc_expr env bindings e2)
  | Me_EFun (args, expr) as e ->
    let fvs = StrSet.to_list (expr_free_vars env e) in
    let body = cc_expr env StrMap.empty expr in
    let e_fun = Me_EFun (fvs @ args, body) in
    List.fold_left (fun acc arg -> Me_EApp (acc, Me_EIdentifier arg)) e_fun fvs
  | Me_ELet (NoRec, name, e1, e2) ->
    let new_e1, bindings =
      match e1 with
      | Me_EFun (args, expr) as e ->
        let fvs = StrSet.to_list (expr_free_vars env e) in
        let body = cc_expr env StrMap.empty expr in
        let new_expr = Me_EFun (fvs @ args, body) in
        let apply =
          List.fold_left
            (fun acc arg -> Me_EApp (acc, Me_EIdentifier arg))
            (Me_EIdentifier name)
            fvs
        in
        new_expr, StrMap.update bindings name ~f:(fun _ -> apply)
      | expr -> cc_expr env StrMap.empty expr, bindings
    in
    let new_e2 = cc_expr env bindings e2 in
    Me_ELet (NoRec, name, new_e1, new_e2)
  | Me_ELet (Rec, name, e1, e2) ->
    let new_e1, bindings =
      match e1 with
      | Me_EFun (args, expr) as e ->
        let fvs = StrSet.to_list (expr_free_vars (StrSet.add env name) e) in
        let apply =
          List.fold_left
            (fun acc arg -> Me_EApp (acc, Me_EIdentifier arg))
            (Me_EIdentifier name)
            fvs
        in
        let body = cc_expr env (StrMap.singleton name apply) expr in
        let new_expr = Me_EFun (fvs @ args, body) in
        new_expr, StrMap.update bindings name ~f:(fun _ -> apply)
      | expr -> cc_expr env StrMap.empty expr, bindings
    in
    let new_e2 = cc_expr (StrSet.add env name) bindings e2 in
    Me_ELet (Rec, name, new_e1, new_e2)
  | expr -> expr
;;

let cc_decl env = function
  | Me_Nonrec decls ->
    let decls =
      List.map (fun (name, expr) -> name, cc_expr env StrMap.empty expr) decls
    in
    let env = List.fold_left (fun acc (name, _) -> StrSet.add acc name) env decls in
    Me_Nonrec decls, env
  | Me_Rec decls ->
    (* let env = List.fold_left (fun acc (name, _) -> StrSet.add acc name) env decls in *)
    let decls =
      List.map
        (fun (name, expr) ->
          match expr with
          | Me_EFun (args, expr) -> name, Me_EFun (args, cc_expr env StrMap.empty expr)
          | expr -> name, cc_expr env StrMap.empty expr)
        decls
    in
    Me_Rec decls, env
;;

let cc_program ast =
  let builtins = StrSet.of_list builtins in
  let rec helper last_env = function
    | [] -> []
    | hd :: tl ->
      let decls, env = cc_decl last_env hd in
      decls :: helper env tl
  in
  helper builtins ast
;;

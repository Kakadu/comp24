(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Me_ast
open Base
open Common
open StateMonad

let get_new_id n name = Base.String.concat [ name; "_ll"; Int.to_string n ]

let rec ll_expr bindings = function
  | (Me_EUnit | Me_ENill | Me_EConst _) as expr -> return ([], expr)
  | Me_EIdentifier id ->
    (match StrMap.find bindings id with
     | Some name -> return ([], Me_EIdentifier name)
     | None -> return ([], Me_EIdentifier id))
  | Me_EIf (e1, e2, e3) ->
    let* defs1, e1' = ll_expr bindings e1 in
    let* defs2, e2' = ll_expr bindings e2 in
    let* defs3, e3' = ll_expr bindings e3 in
    return (defs1 @ defs2 @ defs3, Me_EIf (e1', e2', e3'))
  | Me_EApp (e1, e2) ->
    let* defs1, e1' = ll_expr bindings e1 in
    let* defs2, e2' = ll_expr bindings e2 in
    return (defs1 @ defs2, Me_EApp (e1', e2'))
  | Me_ECons (e1, e2) ->
    let* defs1, e1' = ll_expr bindings e1 in
    let* defs2, e2' = ll_expr bindings e2 in
    return (defs1 @ defs2, Me_ECons (e1', e2'))
  | Me_ETuple lst ->
    let* results =
      RList.fold_left
        lst
        ~init:(return ([], []))
        ~f:(fun (acc_defs, acc_exprs) e ->
          let* defs, e' = ll_expr bindings e in
          return (acc_defs @ defs, acc_exprs @ [ e' ]))
    in
    let defs, exprs = results in
    return (defs, Me_ETuple exprs)
  | Me_ELet (NoRec, name, e1, e2) ->
    (match e1 with
     | Me_EFun (args, e1) ->
       let* defs1, e1' = ll_expr bindings e1 in
       let* id = fresh in
       let new_name = get_new_id id name in
       let def = Me_Nonrec [ new_name, Me_EFun (args, e1') ] in
       let* defs2, e2' =
         ll_expr (StrMap.update bindings name ~f:(fun _ -> new_name)) e2
       in
       return (defs1 @ [ def ] @ defs2, e2')
     | e1 ->
       let* defs1, e1' = ll_expr bindings e1 in
       let* defs2, e2' = ll_expr bindings e2 in
       return (defs1 @ defs2, Me_ELet (NoRec, name, e1', e2')))
  | Me_ELet (Rec, name, e1, e2) ->
    (match e1 with
     | Me_EFun (args, e1) ->
       let* id = fresh in
       let new_name = get_new_id id name in
       let bindings' = StrMap.update bindings name ~f:(fun _ -> new_name) in
       let* defs1, e1' = ll_expr bindings' e1 in
       let def = Me_Rec [ new_name, Me_EFun (args, e1') ] in
       let* defs2, e2' = ll_expr bindings' e2 in
       return (defs1 @ [ def ] @ defs2, e2')
     | _ -> failwith "Not reachable")
  | Me_EFun (args, body) ->
    let* id = fresh in
    let name = get_new_id id "lam" in
    let* defs, body' = ll_expr bindings body in
    let new_fun = Me_EFun (args, body') in
    let def = Me_Nonrec [ name, new_fun ] in
    return (defs @ [ def ], Me_EIdentifier name)
;;

let ll_binding (name, expr) =
  match expr with
  | Me_EFun (args, expr) ->
    let* defs, expr' = ll_expr StrMap.empty expr in
    return (defs, (name, Me_EFun (args, expr')))
  | expr ->
    let* defs, expr' = ll_expr StrMap.empty expr in
    return (defs, (name, expr'))
;;

let ll_decl decl =
  match decl with
  | Me_Nonrec bindings ->
    let* all_defs, curr_defs =
      RList.fold_left
        bindings
        ~init:(return ([], []))
        ~f:(fun (acc_defs, acc_curr) b ->
          let* defs, binding = ll_binding b in
          return (acc_defs @ defs, acc_curr @ [ binding ]))
    in
    return (all_defs, Me_Nonrec curr_defs)
  | Me_Rec bindings ->
    let* all_defs, curr_defs =
      RList.fold_left
        bindings
        ~init:(return ([], []))
        ~f:(fun (acc_defs, acc_curr) b ->
          let* defs, binding = ll_binding b in
          return (acc_defs @ defs, acc_curr @ [ binding ]))
    in
    return (all_defs, Me_Rec curr_defs)
;;

let lambda_lift prog =
  StateMonad.run
    (RList.fold_left prog ~init:(return []) ~f:(fun acc decl ->
       let* decls, d = ll_decl decl in
       return (acc @ decls @ [ d ])))
;;

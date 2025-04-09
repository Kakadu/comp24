(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Me_ast
open Base
open Common
open StateMonad

let get_new_id n name = Base.String.concat [ name; "_ll"; Int.to_string n ]

(* добавляем в списочек свободные переменные *)
let rec free_vars expr bound =
  match expr with
  | Me_EUnit | Me_ENill | Me_EConst _ -> []
  | Me_EIdentifier x -> if Set.mem bound x then [] else [ x ]
  | Me_EIf (e1, e2, e3) -> free_vars e1 bound @ free_vars e2 bound @ free_vars e3 bound
  | Me_EFun (args, body) ->
    let bound' = List.fold_left ~f:Set.add ~init:bound args in
    free_vars body bound'
  | Me_EApp (e1, e2) -> free_vars e1 bound @ free_vars e2 bound
  | Me_ELet (_, name, e1, e2) ->
    let fv1 = free_vars e1 bound in
    let fv2 = free_vars e2 (Set.add bound name) in
    fv1 @ fv2
  | Me_ECons (e1, e2) -> free_vars e1 bound @ free_vars e2 bound
  | Me_ETuple lst -> List.fold_left lst ~init:[] ~f:(fun acc e -> acc @ free_vars e bound)
;;

let rec ll_expr expr =
  match expr with
  | Me_EUnit | Me_ENill | Me_EConst _ | Me_EIdentifier _ -> return ([], expr)
  | Me_EIf (e1, e2, e3) ->
    let* defs1, e1' = ll_expr e1 in
    let* defs2, e2' = ll_expr e2 in
    let* defs3, e3' = ll_expr e3 in
    return (defs1 @ defs2 @ defs3, Me_EIf (e1', e2', e3'))
  | Me_EApp (e1, e2) ->
    let* defs1, e1' = ll_expr e1 in
    let* defs2, e2' = ll_expr e2 in
    return (defs1 @ defs2, Me_EApp (e1', e2'))
  | Me_ECons (e1, e2) ->
    let* defs1, e1' = ll_expr e1 in
    let* defs2, e2' = ll_expr e2 in
    return (defs1 @ defs2, Me_ECons (e1', e2'))
  | Me_ETuple lst ->
    let* results =
      RList.fold_left
        lst
        ~init:(return ([], []))
        ~f:(fun (acc_defs, acc_exprs) e ->
          let* defs, e' = ll_expr e in
          return (acc_defs @ defs, acc_exprs @ [ e' ]))
    in
    let defs, exprs = results in
    return (defs, Me_ETuple exprs)
  | Me_ELet (flag, name, e1, e2) ->
    let* defs1, e1' = ll_expr e1 in
    let* defs2, e2' = ll_expr e2 in
    (* e1' — это анонимная функция (а функции у нас только так) *)
    (match e1' with
     | Me_EFun (args, body) ->
       let* id = fresh in
       let new_name = get_new_id id name in
       let fvs = free_vars e1' (Set.of_list (module String) args) in
       let new_args = fvs @ args in
       let new_fun = Me_EFun (new_args, body) in
       let def = new_name, new_fun in
       let call_expr =
         List.fold_left
           (List.map ~f:(fun x -> Me_EIdentifier x) fvs)
           ~init:(Me_EIdentifier new_name)
           ~f:(fun acc arg -> Me_EApp (acc, arg))
       in
       return (defs1 @ [ def ] @ defs2, Me_ELet (flag, name, call_expr, e2'))
     | _ -> return (defs1 @ defs2, Me_ELet (flag, name, e1', e2')))
  | Me_EFun (args, body) ->
    let* id = fresh in
    let name = get_new_id id "lam" in
    let bound = Set.of_list (module String) args in
    let fvs = free_vars expr bound in
    let all_args = fvs @ args in
    let* defs, body' = ll_expr body in
    let new_fun = Me_EFun (all_args, body') in
    let def = name, new_fun in
    let call_expr =
      List.fold_left
        (List.map ~f:(fun x -> Me_EIdentifier x) fvs)
        ~init:(Me_EIdentifier name)
        ~f:(fun acc arg -> Me_EApp (acc, arg))
    in
    return (defs @ [ def ], call_expr)
;;

let ll_binding (name, expr) =
  let* defs, expr' = ll_expr expr in
  return (defs @ [ name, expr' ])
;;

let ll_decl decl =
  match decl with
  | Me_Nonrec bindings ->
    let* all_defs =
      RList.fold_left bindings ~init:(return []) ~f:(fun acc b ->
        let* lifted = ll_binding b in
        return (acc @ lifted))
    in
    return (Me_Nonrec all_defs)
  | Me_Rec bindings ->
    RList.fold_left bindings ~init:(return []) ~f:(fun acc (name, expr) ->
      let* defs, expr' = ll_expr expr in
      return (acc @ defs @ [ name, expr' ]))
    >>= fun all -> return (Me_Rec all)
;;

let lambda_lift prog =
  StateMonad.run
    (RList.fold_left prog ~init:(return []) ~f:(fun acc decl ->
       let* d = ll_decl decl in
       return (acc @ [ d ])))
;;

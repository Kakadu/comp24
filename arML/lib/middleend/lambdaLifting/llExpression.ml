(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Llast
open Common.StateMonad
open Common.StateMonad.Syntax
open Common.IdentifierStructs
open Common.IdentifierSearcher

let get_new_ll_name = Common.NameCreator.get_new_name "ll"

let rec ll_expression env replacement_map lifted = function
  | EConstant const -> return @@ (LEConstant const, lifted)
  | EIdentifier id -> ll_identifier replacement_map lifted id
  | EEmptyList -> return (LEEmptyList, lifted)
  | EFun ((p, ps), body) -> ll_fun env replacement_map lifted (p, ps) body
  | EApplication (func, fst_arg, args) ->
    ll_application env replacement_map lifted (func, fst_arg, args)
  | EIfThenElse (c, b1, b2) -> ll_if_then_else env replacement_map lifted (c, b1, b2)
  | EListConstructor (l, r) -> ll_list_constructor env replacement_map lifted (l, r)
  | ETuple (e1, e2, es) -> ll_tuple env replacement_map lifted (e1, e2, es)
  | EMatchWith (e, c, cs) -> ll_match_with env replacement_map lifted (e, c, cs)
  | ETyped (e, t) -> ll_typed_expr env replacement_map lifted (e, t)
  | _ -> 
    let _ = replacement_map in
    let _ = env in
    return (LEEmptyList, lifted)

and ll_fun env replacement_map lifted (p, ps) body =
  let* name = get_new_ll_name env in
  let pattern_identifiers = get_pattern_identifiers_from_list (p :: ps) in
  let new_replacement_map = remove_keys_from_map pattern_identifiers replacement_map in
  let* lifted_body, lifted_exprs = ll_expression env new_replacement_map lifted body in
  let new_expr = LEIdentifier (Id name) in
  let lifted_expr = LDOrdinary ((PVar (Id name), p :: ps, lifted_body), []) in
  return @@ (new_expr, lifted_expr :: lifted_exprs)

and ll_identifier replacement_map lifted id =
  let new_id =
    match IdentifierMap.find_opt id replacement_map with
    | Some name -> LEIdentifier name
    | None -> LEIdentifier id
  in
  return @@ (new_id, lifted)

and ll_application env replacement_map lifted (func, fst_arg, args) =
  let* lifted_func, lifted1 = ll_expression env replacement_map lifted func in
  let* lifted_fst_arg, lifted2 = ll_expression env replacement_map lifted1 fst_arg in
  let* lifted_args, lifted3 =
    List.fold_right
      (fun arg acc ->
         let* acc_exprs, acc_lifted = acc in
         let* lifted_arg, lifted_next =
           ll_expression env replacement_map acc_lifted arg
         in
         return (lifted_arg :: acc_exprs, lifted_next))
      args
      (return ([], lifted2))
  in
  let ll_appl = LEApplication (lifted_func, lifted_fst_arg, lifted_args) in
  return @@ (ll_appl, lifted3)

and ll_if_then_else env replacement_map lifted (c, b1, b2) =
  let* l_c, lifted1 = ll_expression env replacement_map lifted c in
  let* l_b1, lifted2 = ll_expression env replacement_map lifted1 b1 in
  let* l_b2, lifted3 = 
    match b2 with
    | Some expr -> 
      let* l_b2, lifted3 = ll_expression env replacement_map lifted2 expr in
      return @@ (Some l_b2, lifted3)
    | None -> return (None, lifted2)
  in
  return @@ (LEIfThenElse (l_c, l_b1, l_b2), lifted3)

and ll_list_constructor env replacement_map lifted (hd, tl) =
  let* l_hd, lifted1 = ll_expression env replacement_map lifted hd in
  let* l_tl, lifted2 = ll_expression env replacement_map lifted1 tl in
  return @@ (LEListConstructor (l_hd, l_tl), lifted2)

and ll_tuple env replacement_map lifted (e1, e2, es) =
  let* l_e1, lifted1 = ll_expression env replacement_map lifted e1 in
  let* l_e2, lifted2 = ll_expression env replacement_map lifted1 e2 in
  let* l_es, lifted3 =
    List.fold_left
      (fun acc e ->
         let* acc_es, acc_lifted = acc in
         let* l_e, new_acc_lifted = ll_expression env replacement_map acc_lifted e in
         return (l_e :: acc_es, new_acc_lifted))
      (return @@ ([], lifted2))
      es
  in
  let l_es = List.rev l_es in
  return @@ (LETuple (l_e1, l_e2, l_es), lifted3)

and ll_match_with env replacement_map lifted (expr, (p, e), cases) =
  let* l_expr, lifted1 = ll_expression env replacement_map lifted expr in
  let* l_case, lifted2 =
    let* l_e, lifted2 = ll_expression env replacement_map lifted1 e in
    return ((p, l_e), lifted2)
  in
  let* lifted_cases, lifted3 =
    List.fold_right
      (fun (p, e) acc ->
         let* acc_cases, acc_lifted = acc in
         let* l_e, lifted_next = ll_expression env replacement_map acc_lifted e in
         return ((p, l_e) :: acc_cases, lifted_next))
      cases
      (return ([], lifted2))
  in
  return @@ (LEMatchWith (l_expr, l_case, lifted_cases), lifted3)

and ll_typed_expr env replacement_map lifted (e, t) =
  let* l_e, lifted1 = ll_expression env replacement_map lifted e in
  return @@ (LETyped (l_e, t), lifted1)
;;

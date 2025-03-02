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
;;

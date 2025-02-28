(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Common.IdentifierStructs

let rec get_pattern_identifiers = function
  | PAny | PNill | PConst _ -> IdentifierSet.empty
  | PVar id -> IdentifierSet.singleton id
  | PTuple (p1, p2, ps) ->
    let idf = List.map get_pattern_identifiers (p1 :: p2 :: ps) in
    List.fold_left IdentifierSet.union IdentifierSet.empty idf
  | PListConstructor (p1, p2) ->
    IdentifierSet.union (get_pattern_identifiers p1) (get_pattern_identifiers p2)
  | PTyped (p, _) -> get_pattern_identifiers p
;;

let get_pattern_identifiers_from_list lst =
  List.fold_left
    (fun acc p -> IdentifierSet.union acc (get_pattern_identifiers p))
    IdentifierSet.empty
    lst
;;

let get_pattern_identifiers_from_cases cases =
  let patterns = List.map (fun (p, _) -> p) cases in
  get_pattern_identifiers_from_list patterns
;;

let rec get_expr_free_vars = function
  | EConstant _ -> IdentifierSet.empty
  | EIdentifier id -> IdentifierSet.singleton id
  | EFun ((p, ps), body) -> get_fun_free_vars (p :: ps) body
  | EFunction (case, cases) -> get_function_free_vars (case :: cases)
  | EApplication (func, arg1, args) -> get_application_free_vars func (arg1 :: args)
  | EIfThenElse (cond, b1, b2) -> get_if_then_else_free_vars cond b1 b2
  | EEmptyList -> IdentifierSet.empty
  | EListConstructor (e1, e2) ->
    IdentifierSet.union (get_expr_free_vars e1) (get_expr_free_vars e2)
  | ETuple (e1, e2, es) -> get_tuple_free_vars (e1 :: e2 :: es)
  | EMatchWith (e, case, cases) -> get_match_with_free_vars e (case :: cases)
  | ELetIn (case, cases, e) -> get_let_in_free_vars (case :: cases) e
  | ERecLetIn (case, cases, e) -> get_rec_let_in_free_vars (case :: cases) e
  | ETyped (e, _) -> get_expr_free_vars e

and get_case_free_vars (p, e) =
  let bound_vars = get_pattern_identifiers p in
  IdentifierSet.diff (get_expr_free_vars e) bound_vars

and get_fun_free_vars ps body =
  let body_free_vars = get_expr_free_vars body in
  let patterns_identifiers = get_pattern_identifiers_from_list ps in
  IdentifierSet.diff body_free_vars patterns_identifiers

and get_function_free_vars cases =
  List.fold_left
    IdentifierSet.union
    IdentifierSet.empty
    (List.map get_case_free_vars cases)

and get_application_free_vars func args =
  let func_free_vars = get_expr_free_vars func in
  let args_free_vars = List.map get_expr_free_vars args in
  let args_free_vars =
    List.fold_left IdentifierSet.union IdentifierSet.empty args_free_vars
  in
  IdentifierSet.union func_free_vars args_free_vars

and get_if_then_else_free_vars cond b1 b2 =
  let cond_vars = get_expr_free_vars cond in
  let b1_vars = get_expr_free_vars b1 in
  let b2_vars =
    Option.map get_expr_free_vars b2 |> Option.value ~default:IdentifierSet.empty
  in
  IdentifierSet.union cond_vars (IdentifierSet.union b1_vars b2_vars)

and get_tuple_free_vars es =
  let vars = List.map get_expr_free_vars es in
  List.fold_left IdentifierSet.union IdentifierSet.empty vars

and get_match_with_free_vars e cases =
  let e_vars = get_expr_free_vars e in
  let cases_vars = List.map get_case_free_vars cases in
  List.fold_left IdentifierSet.union e_vars cases_vars

and get_let_in_free_vars cases e =
  let get_case_bound_vars (p, _) = get_pattern_identifiers p in
  let bound_vars = List.map get_case_bound_vars cases in
  let bound_vars = List.fold_left IdentifierSet.union IdentifierSet.empty bound_vars in
  let cases_free_vars = List.map get_case_free_vars cases in
  let cases_free_vars =
    List.fold_left IdentifierSet.union IdentifierSet.empty cases_free_vars
  in
  let body_free_vars = IdentifierSet.diff (get_expr_free_vars e) bound_vars in
  IdentifierSet.union cases_free_vars body_free_vars

and get_rec_let_in_free_vars cases e =
  let bound_vars =
    List.fold_left
      (fun acc (p, _) -> IdentifierSet.union acc (get_pattern_identifiers p))
      IdentifierSet.empty
      cases
  in
  let cases_free_vars =
    List.fold_left
      (fun acc (_, expr) ->
        let free_vars = IdentifierSet.diff (get_expr_free_vars expr) bound_vars in
        IdentifierSet.union acc free_vars)
      IdentifierSet.empty
      cases
  in
  let body_free_vars = IdentifierSet.diff (get_expr_free_vars e) bound_vars in
  IdentifierSet.union cases_free_vars body_free_vars
;;

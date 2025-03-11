(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.IdentifierStructs
open Pmfast

let rec collect_identifiers_expr (expr : pmf_expression) (acc : IdentifierSet.t)
  : IdentifierSet.t
  =
  match expr with
  | PMFConstant _ -> acc
  | PMFIdentifier id -> IdentifierSet.add id acc
  | PMFApplication (func, arg, args) ->
    let acc = collect_identifiers_expr func acc in
    let acc = collect_identifiers_expr arg acc in
    List.fold_left (fun acc e -> collect_identifiers_expr e acc) acc args
  | PMFLetIn ((id, body), in_expr) ->
    let acc = IdentifierSet.add id acc in
    let acc = collect_identifiers_expr body acc in
    collect_identifiers_expr in_expr acc
  | PMFIfThenElse (cond, then_expr, else_expr) ->
    let acc = collect_identifiers_expr cond acc in
    let acc = collect_identifiers_expr then_expr acc in
    (match else_expr with
     | None -> acc
     | Some e -> collect_identifiers_expr e acc)
  | PMFTuple (e1, e2, es) ->
    let acc = collect_identifiers_expr e1 acc in
    let acc = collect_identifiers_expr e2 acc in
    List.fold_left (fun acc e -> collect_identifiers_expr e acc) acc es
  | PMFListConstructor (e1, e2) ->
    let acc = collect_identifiers_expr e1 acc in
    collect_identifiers_expr e2 acc
  | PMFEmptyList -> acc
  | PMFTyped (e, _) -> collect_identifiers_expr e acc
;;

let collect_identifiers_decl (decl : pmf_decl) (acc : IdentifierSet.t) : IdentifierSet.t =
  match decl with
  | PMFDOrdinary (id, args, body) ->
    let acc = IdentifierSet.add id acc in
    let acc = List.fold_left (fun acc arg -> IdentifierSet.add arg acc) acc args in
    collect_identifiers_expr body acc
  | PMFDRecursive ((id, args, body), decls) ->
    let acc = IdentifierSet.add id acc in
    let acc = List.fold_left (fun acc arg -> IdentifierSet.add arg acc) acc args in
    let acc = collect_identifiers_expr body acc in
    List.fold_left
      (fun acc (id, args, body) ->
        let acc = IdentifierSet.add id acc in
        let acc = List.fold_left (fun acc arg -> IdentifierSet.add arg acc) acc args in
        collect_identifiers_expr body acc)
      acc
      decls
;;

let collect_all_identifiers (initial_set : IdentifierSet.t) (program : pmf_program)
  : IdentifierSet.t
  =
  List.fold_left (fun acc decl -> collect_identifiers_decl decl acc) initial_set program
;;

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Common.IdentifierStructs
open IdentifierSearcher

let rec substitute_identifiers replacement_map expr =
  match expr with
  | EIdentifier id ->
    (match IdentifierMap.find_opt id replacement_map with
     | Some new_id -> EIdentifier new_id
     | None -> expr)
  | EFun ((p, ps), body) ->
    let patterns_identifiers = List.fold_left (fun acc p -> IdentifierSet.union acc (get_pattern_identifiers p)) IdentifierSet.empty (p :: ps) in
    let body' = substitute_identifiers (remove_keys_from_map patterns_identifiers replacement_map) body in
    EFun ((p, ps), body')
  | EApplication (f, arg1, args) ->
    let f' = substitute_identifiers replacement_map f in
    let arg1' = substitute_identifiers replacement_map arg1 in
    let args' = List.map (substitute_identifiers replacement_map) args in
    EApplication (f', arg1', args')
  | EListConstructor (l, r) ->
    let l = substitute_identifiers replacement_map l in
    let r = substitute_identifiers replacement_map r in
    EListConstructor (l, r)
  | EIfThenElse (cond, b1, b2) ->
    let cond = substitute_identifiers replacement_map cond in
    let b1 = substitute_identifiers replacement_map b1 in
    let b2 = (match b2 with
        | Some b2 -> Some (substitute_identifiers replacement_map b2)
        | None -> None)
    in
    EIfThenElse (cond, b1, b2)
  | ETuple (e1, e2, es) ->
    let e1 = substitute_identifiers replacement_map e1 in
    let e2 = substitute_identifiers replacement_map e2 in
    let es = List.map (fun e -> substitute_identifiers replacement_map e) es in
    ETuple (e1, e2, es)
  | EFunction (case, cases) ->
    let cases' = List.map (fun (p, e) -> (p, substitute_identifiers (remove_keys_from_map (get_pattern_identifiers p) replacement_map) e)) (case :: cases) in
    EFunction (List.hd cases', List.tl cases')
  | EMatchWith (expr, case, cases) ->
    let expr = substitute_identifiers replacement_map expr in
    let cases' = List.map (fun (p, e) -> (p, substitute_identifiers (remove_keys_from_map (get_pattern_identifiers p) replacement_map) e)) (case :: cases) in
    EMatchWith (expr, List.hd cases', List.tl cases')
  | ETyped (e, t) ->
    let e' = substitute_identifiers replacement_map e in
    ETyped (e', t)
  | ELetIn (case, cases, body) ->
    let patterns_identifiers = get_pattern_identifiers_from_cases (case :: cases) in
    let cases' = List.map (fun (p, e) -> (p, substitute_identifiers replacement_map e)) (case :: cases) in
    let body' = substitute_identifiers (remove_keys_from_map patterns_identifiers replacement_map) body in
    ELetIn (List.hd cases', List.tl cases', body')
  | ERecLetIn (case, cases, body) ->
    let patterns_identifiers = get_pattern_identifiers_from_cases (case :: cases) in
    let replacement_map = remove_keys_from_map patterns_identifiers replacement_map in
    let cases' = List.map (fun (p, e) -> (p, substitute_identifiers replacement_map e)) (case :: cases) in
    let body' = substitute_identifiers replacement_map body in
    ERecLetIn (List.hd cases', List.tl cases', body')
  | _ -> expr
;;

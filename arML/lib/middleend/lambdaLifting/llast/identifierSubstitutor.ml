(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llast
open Common.IdentifierStructs
open Common.IdentifierSearcher

let get_pattern_identifiers_from_cases cases =
  let patterns = List.map (fun (p, _) -> p) cases in
  get_pattern_identifiers_from_list patterns
;;

let rec substitute_identifiers_ll replacement_map expr =
  match expr with
  | LEIdentifier id ->
    (match IdentifierMap.find_opt id replacement_map with
     | Some new_id -> LEIdentifier new_id
     | None -> expr)
  | LEApplication (f, arg1, args) ->
    let f' = substitute_identifiers_ll replacement_map f in
    let arg1' = substitute_identifiers_ll replacement_map arg1 in
    let args' = List.map (substitute_identifiers_ll replacement_map) args in
    LEApplication (f', arg1', args')
  | LELetIn (case, cases, body) ->
    let patterns_identifiers = get_pattern_identifiers_from_cases (case :: cases) in
    let cases' =
      List.map (fun (p, e) -> p, substitute_identifiers_ll replacement_map e) (case :: cases)
    in
    let body' =
      substitute_identifiers_ll
        (remove_keys_from_map patterns_identifiers replacement_map)
        body
    in
    LELetIn (List.hd cases', List.tl cases', body')
  | LEIfThenElse (cond, b1, b2) ->
    let cond' = substitute_identifiers_ll replacement_map cond in
    let b1' = substitute_identifiers_ll replacement_map b1 in
    let b2' = Option.map (substitute_identifiers_ll replacement_map) b2 in
    LEIfThenElse (cond', b1', b2')
  | LETuple (e1, e2, es) ->
    let e1' = substitute_identifiers_ll replacement_map e1 in
    let e2' = substitute_identifiers_ll replacement_map e2 in
    let es' = List.map (substitute_identifiers_ll replacement_map) es in
    LETuple (e1', e2', es')
  | LEListConstructor (l, r) ->
    let l' = substitute_identifiers_ll replacement_map l in
    let r' = substitute_identifiers_ll replacement_map r in
    LEListConstructor (l', r')
  | LEMatchWith (expr, case, cases) ->
    let expr' = substitute_identifiers_ll replacement_map expr in
    let cases' =
      List.map
        (fun (p, e) ->
           ( p
           , substitute_identifiers_ll
               (remove_keys_from_map (get_pattern_identifiers p) replacement_map)
               e ))
        (case :: cases)
    in
    LEMatchWith (expr', List.hd cases', List.tl cases')
  | LETyped (e, t) ->
    let e' = substitute_identifiers_ll replacement_map e in
    LETyped (e', t)
  | _ -> expr
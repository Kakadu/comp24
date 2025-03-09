(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateMonad
open Common.StateMonad.Syntax
open PatternMatchingElim.Pmfast
open NameGenerator

let rec expression_alpha_conversion env replacement_map = function
  | PMFConstant c -> return @@ PMFConstant c
  | PMFIdentifier v ->  replace_name_if_needed v replacement_map
  | PMFEmptyList -> return @@ PMFEmptyList
  | PMFApplication (a1, a2, args) ->
    let* a1' = expression_alpha_conversion env replacement_map a1 in
    let* a2' = expression_alpha_conversion env replacement_map a2 in
    let* args' =
      List.fold_right
        (fun arg acc ->
           let* acc = acc in
           let* arg' = expression_alpha_conversion env replacement_map arg in
           return (arg' :: acc))
        args
        (return [])
    in
    return @@ (PMFApplication (a1', a2', args'))
  | PMFIfThenElse (c, b1, b2) ->
    let* c' = expression_alpha_conversion env replacement_map c in
    let* b1' = expression_alpha_conversion env replacement_map b1 in
    let* b2' =
      (match b2 with
       | Some b2 -> 
         let* b2' = expression_alpha_conversion env replacement_map b2 in
         return @@ ((Some b2'))
       | None -> return @@ (None))
    in
    return @@ (PMFIfThenElse (c', b1', b2'))
  | PMFTuple (e1, e2, es) ->
    let* e1' = expression_alpha_conversion env replacement_map e1 in
    let* e2' = expression_alpha_conversion env replacement_map e2 in
    let* es' =
      List.fold_right
        (fun e acc ->
           let* acc = acc in
           let* e' = expression_alpha_conversion env replacement_map e in
           return (e' :: acc))
        es
        (return ([]))
    in
    return @@ (PMFTuple (e1', e2', es'))
  | PMFListConstructor (l, r) ->
    let* l' = expression_alpha_conversion env replacement_map l in
    let* r' = expression_alpha_conversion env replacement_map r in
    return @@ (PMFListConstructor (l', r'))
  | PMFLetIn ((name, expr1), expr2) ->
    let* new_name, env, replacement_map' = generate_new_name_if_needed name env replacement_map in
    let* expr1' = expression_alpha_conversion env replacement_map expr1 in
    let* expr2' = expression_alpha_conversion env replacement_map' expr2 in
    return @@ (PMFLetIn ((new_name, expr1'), expr2'))
  | PMFTyped (expr, typ) ->
    let* expr' = expression_alpha_conversion env replacement_map expr in
    return @@ (PMFTyped (expr', typ))
;;

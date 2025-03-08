(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateMonad
open Common.StateMonad.Syntax
open PatternMatchingElim.Pmfast
open NameGenerator

let rec expression_alpha_conversion env replacement_map = function
  | PMFConstant c -> return @@ (PMFConstant c, env, replacement_map)
  | PMFIdentifier v ->  replace_name_if_needed v env replacement_map
  | PMFEmptyList -> return @@ (PMFEmptyList, env, replacement_map)
  | PMFApplication (a1, a2, args) ->
    let* a1', env, replacement_map = expression_alpha_conversion env replacement_map a1 in
    let* a2', env, replacement_map = expression_alpha_conversion env replacement_map a2 in
    let* args', env, replacement_map =
      List.fold_right
        (fun arg acc ->
           let* acc, env, replacement_map = acc in
           let* arg', env, replacement_map = expression_alpha_conversion env replacement_map arg in
           return (arg' :: acc, env, replacement_map))
        args
        (return ([], env, replacement_map))
    in
    return @@ (PMFApplication (a1', a2', args'), env, replacement_map)
  | PMFIfThenElse (c, b1, b2) ->
    let* c', env, replacement_map = expression_alpha_conversion env replacement_map c in
    let* b1', env, replacement_map = expression_alpha_conversion env replacement_map b1 in
    let* b2', env, replacement_map =
      (match b2 with
       | Some b2 -> 
         let* b2', env, replacement_map = expression_alpha_conversion env replacement_map b2 in
         return @@ ((Some b2'), env, replacement_map)
       | None -> return @@ (None, env, replacement_map))
    in
    return @@ (PMFIfThenElse (c', b1', b2'), env, replacement_map)
  | PMFTuple (e1, e2, es) ->
    let* e1', env, replacement_map = expression_alpha_conversion env replacement_map e1 in
    let* e2', env, replacement_map = expression_alpha_conversion env replacement_map e2 in
    let* es', env, replacement_map =
      List.fold_right
        (fun e acc ->
           let* acc, env, replacement_map = acc in
           let* e', env, replacement_map = expression_alpha_conversion env replacement_map e in
           return (e' :: acc, env, replacement_map))
        es
        (return ([], env, replacement_map))
    in
    return @@ (PMFTuple (e1', e2', es'), env, replacement_map)
  | PMFListConstructor (l, r) ->
    let* l', env, replacement_map = expression_alpha_conversion env replacement_map l in
    let* r', env, replacement_map = expression_alpha_conversion env replacement_map r in
    return @@ (PMFListConstructor (l', r'), env, replacement_map)
  | PMFLetIn ((name, expr1), expr2) ->
    let* new_name, env, replacement_map = generate_new_name_if_needed name env replacement_map in
    let* expr1', env, replacement_map = expression_alpha_conversion env replacement_map expr1 in
    let* expr2', env, replacement_map = expression_alpha_conversion env replacement_map expr2 in
    return @@ (PMFLetIn ((new_name, expr1'), expr2'), env, replacement_map)
  | PMFTyped (expr, typ) ->
    let* expr', env, replacement_map = expression_alpha_conversion env replacement_map expr in
    return @@ (PMFTyped (expr', typ), env, replacement_map)
;;

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateMonad
open Common.StateMonad.Syntax
open Common.IdentifierStructs
open PatternMatchingElim.Pmfast
open NameGenerator
open ExpressionAlphaConversion

let declaration_alpha_conversion env replacement_map = function
  | PMFDOrdinary (name, args, body) ->
    let* name', env, replacement_map' = generate_new_name_if_needed name env replacement_map in
    let* args', env, replacement_map'' =
      List.fold_right
        (fun arg_name acc ->
           let* acc, env, replacement_map = acc in
           let* new_arg_name, env, replacement_map = generate_new_name_if_needed arg_name env replacement_map in
           return (new_arg_name :: acc, env, replacement_map))
        args
        (return ([], env, replacement_map))
    in
    let* body' = expression_alpha_conversion env replacement_map'' body in
    return @@ (PMFDOrdinary (name', args', body'), env, replacement_map')
  | PMFDRecursive (case, cases) ->
    let cases = case :: cases in
    let fun_names = 
      List.fold_left
        (fun acc (name, _, _) -> IdentifierSet.add name acc)
        IdentifierSet.empty
        cases
    in
    let replacement_map = remove_keys_from_map fun_names replacement_map in
    let bypass_func (name, args, body) acc = 
      let* acc, env, replacement_map, replacement_map_to_save = acc in
      let* _, _, replacement_map_to_save = generate_new_name_if_needed name env replacement_map_to_save in
      let* name', env, replacement_map = generate_new_name_if_needed name env replacement_map in
      let* args', env, replacement_map =
        List.fold_right
          (fun arg_name acc ->
             let* acc, env, replacement_map = acc in
             let* new_arg_name, env, replacement_map = generate_new_name_if_needed arg_name env replacement_map in
             return (new_arg_name :: acc, env, replacement_map))
          args
          (return ([], env, replacement_map))
      in
      let* body' = expression_alpha_conversion env replacement_map body in
      return @@ ((name', args', body') :: acc, env, replacement_map, replacement_map_to_save)
    in
    let* cases', env, replacement_map, to_save =
      List.fold_right
        bypass_func
        cases
        (return ([], env, replacement_map, replacement_map))
    in
    let* cases', _, _, to_save =
      List.fold_right
        bypass_func
        cases'
        (return ([], IdentifierSet.empty, replacement_map, to_save))
    in
    return @@ (PMFDRecursive (List.hd cases', List.tl cases'), env, to_save)
;;

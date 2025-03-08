(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateMonad
open Common.StateMonad.Syntax
open Common.IdentifierStructs
open Ast.AbstractSyntaxTree
open PatternMatchingElim.Pmfast

let get_new_ac_name = Common.NameCreator.get_new_name "ac"

let generate_new_name name env replacement_map =
  let* new_name = get_new_ac_name env in
  let new_name = Id new_name in
  let new_replacement_map = IdentifierMap.add name new_name replacement_map in
  let new_env = IdentifierSet.add new_name env in
  return @@ (new_name, new_env, new_replacement_map)
;;

let generate_new_name_if_needed name env replacement_map =
  if IdentifierSet.mem name env
  then
    generate_new_name name env replacement_map
  else 
    let new_env = IdentifierSet.add name env in
    return @@ (name, new_env, replacement_map)
;;

let replace_name_if_needed name env replacement_map =
  match IdentifierMap.find_opt name replacement_map with
  | None -> return @@ (PMFIdentifier name, env, replacement_map)
  | Some new_name -> return @@ (PMFIdentifier new_name, env, replacement_map)
;;

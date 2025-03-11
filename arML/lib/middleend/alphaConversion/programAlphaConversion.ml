(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateMonad
open Common.StateMonad.Syntax
open Common.IdentifierStructs
open DeclarationAlphaConversion

let program_alpha_conversion start_env program =
  let rec helper env replacement_map = function
    | [] -> return @@ ([], env, replacement_map)
    | hd :: tl ->
      let* hd', env, replacement_map =
        declaration_alpha_conversion env replacement_map hd
      in
      let* tl', env, replacement_map = helper env replacement_map tl in
      return @@ (hd' :: tl', env, replacement_map)
  in
  let* result, _, _ = helper start_env IdentifierMap.empty program in
  return result
;;

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateMonad
open Common.StateMonad.Syntax
open DeclarationToAnf

let program_to_anf env program =
  let rec helper env = function
    | [] -> return ([], env)
    | hd :: tl ->
      let* d1, env = declaration_to_anf env hd in
      let* d2, env = helper env tl in
      return @@ (d1 @ d2, env)
  in
  helper env program
;;

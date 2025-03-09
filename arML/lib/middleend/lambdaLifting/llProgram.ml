(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateMonad
open Common.StateMonad.Syntax
open Common.IdentifierStructs
open LlDeclaration

let ll_program start_env program =
  let env = start_env program in
  let lifted = [] in
  let replacement_map = IdentifierMap.empty in
  let* lifted_decls =
    List.fold_right
      (fun item acc ->
         let* acc = acc in
         let* lifted_decl, lifted_e = ll_decl env replacement_map lifted item in
         return (((List.rev lifted_e) @ [ lifted_decl ] @ acc)))
      program
      (return ([]))
  in
  return (lifted_decls)
;;

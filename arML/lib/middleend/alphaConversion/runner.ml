(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateMonad
open Common.IdentifierStructs
open ProgramAlphaConversion

let start_env =
  let start_identifiers =
    List.fold_left
      (fun acc var -> IdentifierSet.add (Id var) acc)
      IdentifierSet.empty
      Common.StartEnvironment.start_identifiers
  in
  start_identifiers
;;

let run_alpha_conversion_program program =
  let first_state = 0 in
  let m = program_alpha_conversion start_env program in
  let result = run m first_state in
  fst result
;;

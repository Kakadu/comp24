(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateMonad
open Common.IdentifierStructs
open PatternMatchingElim.CollectIdentifiers
open ProgramToAnf

let start_env program =
  let start_identifiers =
    List.fold_left
      (fun acc var -> IdentifierSet.add (Id var) acc)
      IdentifierSet.empty
      Common.StartEnvironment.start_identifiers
  in
  collect_all_identifiers start_identifiers program
;;

let run_anf_conversion_program program =
  let first_state = 0 in
  let m = program_to_anf (start_env program) program in
  let result = run m first_state in
  fst (fst result)
;;

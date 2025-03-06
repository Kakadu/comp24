(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateMonad
open Common.IdentifierStructs
open ClosureExpression
open ClosureProgram
open Common.StartEnvironment

let start_env =
  List.fold_left
    (fun acc var -> IdentifierSet.add (Id var) acc)
    IdentifierSet.empty
    start_identifiers
;;

let run_closure_expression expression =
  let first_state = 0 in
  let m = closure_expression start_env IdentifierMap.empty expression in
  let result = run m first_state in
  fst result
;;

let run_closure_program program =
  let first_state = 0 in
  let m = closure_program start_env program in
  let result = run m first_state in
  fst result
;;

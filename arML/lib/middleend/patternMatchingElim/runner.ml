(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open PmfProgram
open Common.StateMonad
open Common.IdentifierStructs
open LambdaLifting.Llast
open LambdaLifting.IdentifierSubstitutor

let start_env program =
  let start_identifiers =
    List.fold_left
      (fun acc var -> IdentifierSet.add (Id var) acc)
      IdentifierSet.empty
      Common.StartEnvironment.start_identifiers
  in
  List.fold_left
    (fun acc decl ->
       match decl with
       | LDOrdinary (case, cases) | LDRecursive (case, cases) ->
         let cases = case :: cases in
         let cases = List.fold_right (fun (p, _, e) acc -> (p, e) :: acc) cases [] in
         IdentifierSet.union acc (get_pattern_identifiers_from_cases cases))
    start_identifiers
    program
;;

let run_pmf_program program =
  let first_state = 0 in
  let m = eliminate_pm_program (start_env program) program in
  let result = run m first_state in
  fst (fst result)
;;

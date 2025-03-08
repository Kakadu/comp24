(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateMonad
open Common.StateMonad.Syntax
open Common.IdentifierStructs
open Ast.AbstractSyntaxTree
open ImmutBasicFunctions
open Pmfast
open PmfCase
open PmfExpression

let eliminate_pm_fun env args expr =

  let* env, processed_args, match_args, patterns =
    let rev = List.rev in
    let* env, processed_args, match_args, patterns =
      List.fold_left 
        (fun acc arg ->
           let* (env, processed_args, match_args, patterns) = acc in
           match arg with
           | PVar var_name -> return (env, var_name :: processed_args, match_args, patterns)
           | PConst CUnit -> return (env, Id "()" :: processed_args, match_args, patterns)
           | _ ->
             let* fresh_id = get_new_pmf_name env in
             let fresh_id = Id fresh_id in
             let env = IdentifierSet.add fresh_id env in
             return (env, fresh_id :: processed_args, fresh_id :: match_args, arg :: patterns)) 
        (return (env, [], [], [])) 
        args
    in
    return @@ (env, rev processed_args, rev match_args, rev patterns)
  in

  let* new_body, env = eliminate_pm_expression env expr in

  match match_args with
  | [] -> return ((processed_args, new_body), env)
  | [single_match_arg] ->
    let pat = List.hd patterns in
    let to_match = PMFIdentifier single_match_arg in
    let case_expr = eliminate_pm_case (pat, to_match) (new_body, pattern_matching_failure_flag) in
    return ((processed_args, case_expr), env)
  | _ ->
    (* Сombining complex patterns into one. *)
    let new_pat = PTuple (List.hd patterns, List.nth patterns 1, List.tl (List.tl patterns)) in
    let to_match =
      let match_values = List.map (fun arg -> PMFIdentifier arg) match_args in
      match match_values with
      | first :: second :: rest -> PMFTuple (first, second, rest)
      | _ -> failwith "Unexpected empty list in to_match"
    in
    let* fresh_id = get_new_pmf_name env in
    let fresh_id = Id fresh_id in
    let env = IdentifierSet.add fresh_id env in
    let case_expr = eliminate_pm_case (new_pat, PMFIdentifier fresh_id) (new_body, pattern_matching_failure_flag) in
    return ((processed_args, PMFLetIn ((fresh_id, to_match), case_expr)), env)
;;

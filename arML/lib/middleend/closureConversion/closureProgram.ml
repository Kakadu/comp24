(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Common.StateMonad
open Common.StateMonad.Syntax
open Common.IdentifierStructs
open Common.IdentifierSearcher
open ClosureExpression

let closure_program start_env program =
  let new_declaration decl cases =
    match decl with
    | DOrdinary _ -> DOrdinary (List.hd cases, List.tl cases)
    | DRecursive _ -> DRecursive (List.hd cases, List.tl cases)
  in
  let update_env_with_cases env cases =
    List.fold_left
      (fun acc (p, _) -> IdentifierSet.union acc (get_pattern_identifiers p))
      env
      cases
  in
  let process_cases env cases fv_map =
    List.fold_left
      (fun acc (pattern, body) ->
        let* acc = acc in
        match body with
        | EFun _ ->
          let transformed_body = Common.FunctionTransformer.transform_fun body in
          let* closure_body, _ = closure_fun false env fv_map transformed_body in
          return ((pattern, closure_body) :: acc)
        | _ ->
          let* closure_body = closure_expression env fv_map body in
          return ((pattern, closure_body) :: acc))
      (return [])
      cases
  in
  let rec helper acc env = function
    | [] -> return (List.rev acc)
    | hd :: tl ->
      (match hd with
       | DOrdinary (case, cases) as decl ->
         let* processed_cases = process_cases env (case :: cases) IdentifierMap.empty in
         let updated_env = update_env_with_cases env (case :: cases) in
         let new_decl = new_declaration decl (List.rev processed_cases) in
         let new_acc = new_decl :: acc in
         helper new_acc updated_env tl
       | DRecursive (case, cases) as decl ->
         let updated_env = update_env_with_cases env (case :: cases) in
         let* processed_cases =
           process_cases updated_env (case :: cases) IdentifierMap.empty
         in
         let new_decl = new_declaration decl (List.rev processed_cases) in
         let new_acc = new_decl :: acc in
         helper new_acc updated_env tl)
  in
  helper [] start_env program
;;

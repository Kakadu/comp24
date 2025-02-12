(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Common.StateMonad
open Common.StateMonad.Syntax
open Common.IdentifierStructs
open IdentifierSearcher
open ClosureExpression

let start_identifiers = 
  let binary_operations =
    [ "( + )" 
    ; "( - )" 
    ; "( * )" 
    ; "( / )" 
    ; "( % )" 
    ; "( > )" 
    ; "( < )" 
    ; "( >= )" 
    ; "( <= )" 
    ; "( = )" 
    ; "( != )" 
    ; "( <> )" 
    ; "( && )" 
    ; "( || )"
    ]
  in

  let unary_operations =
    [ "U-"
    ; "U+"
    ; "UNot"
    ]
  in

  let stdlib_functions =
    [ "print_int"
    ; "print_bool"
    ]
  in

  binary_operations @ unary_operations @ stdlib_functions
;;

let start_env = 
  List.fold_left 
    (fun acc var -> IdentifierSet.add (Id var) acc) 
    IdentifierSet.empty 
    start_identifiers
;;

let closure_program program =

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
         (match body with
          | EFun _ ->
            let transformed_body = FunctionTransformer.transform_fun body in
            let* closure_body, _ = closure_fun false env fv_map transformed_body in
            return ((pattern, closure_body) :: acc)
          | _ -> 
            let* closure_body = closure_expression env fv_map body in 
            return ((pattern, closure_body) :: acc)))
      (return [])
      cases
  in

  let rec helper acc env = function
    | [] -> return (List.rev acc)
    | hd :: tl ->
      match hd with
      | DOrdinary (case, cases) as decl ->
        let* processed_cases = process_cases env (case :: cases) IdentifierMap.empty in
        let updated_env = update_env_with_cases env (case :: cases) in
        let new_decl = new_declaration decl (List.rev processed_cases) in
        let new_acc = new_decl :: acc in
        helper new_acc updated_env tl
      | DRecursive (case, cases) as decl ->
        let updated_env = update_env_with_cases env (case :: cases) in
        let* processed_cases = process_cases updated_env (case :: cases) IdentifierMap.empty in
        let new_decl = new_declaration decl (List.rev processed_cases) in
        let new_acc = new_decl :: acc in
        helper new_acc updated_env tl
  in
  helper [] start_env program
;;

let run_closure_program program = 
  let first_state = 0 in
  let m = closure_program program in
  let result = run m first_state in
  fst result
;;

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateMonad
open Common.StateMonad.Syntax
open Common.IdentifierStructs
open LambdaLifting.Llast
open Pmfast
open Ast.AbstractSyntaxTree
open PmfExpression
open GenerateConditionsFromCase
open GenerateDeclarationsFromCase
open ImmutBasicFunctions
open PmfFunction

let eliminate_pm_declaration env = function
  | LDOrdinary (((_, [], _) as case), _) ->
    let process_case env (pat, _, e) =
      let* e, env = eliminate_pm_expression env e in
      match pat with
      | PVar name -> return ([ name, [], e ], env)
      | PConst CUnit -> return ([ Id "()", [], e ], env)
      | _ ->
        let* name = get_new_pmf_name env in
        let name = Id name in
        let env = IdentifierSet.add name env in
        let to_unpack = PMFIdentifier name in
        let new_decls = generate_declarations_from_case (pat, to_unpack) in
        let new_decls = List.map (fun (p, e) -> p, [], e) new_decls in
        let cond =
          generate_condition_if_needed
            (pat, e)
            (PMFConstant CUnit, pattern_matching_failure_flag)
        in
        (match cond with
         | PMFConstant CUnit -> return ((name, [], e) :: new_decls, env)
         | _ -> return ((name, [], e) :: (Id "()", [], cond) :: new_decls, env))
    in
    let* transformed_cases, env = process_case env case in
    let cases = List.map (fun c -> PMFDOrdinary c) transformed_cases in
    return (cases, env)
  | LDOrdinary ((p, args, expr), _) ->
    let name =
      match p with
      | PVar v -> v
      | PConst CUnit -> Id "()"
      | _ -> Id ""
    in
    let* (new_args, new_expr), env = eliminate_pm_fun env args expr in
    return @@ ([ PMFDOrdinary (name, new_args, new_expr) ], env)
  | LDRecursive (case, cases) ->
    let cases = case :: cases in
    let* new_cases, env =
      List.fold_left
        (fun acc (p, ps, body) ->
          let* acc, env = acc in
          let name =
            match p with
            | PVar v -> v
            | PConst CUnit -> Id "()"
            | _ -> Id ""
          in
          let* (new_args, new_expr), env = eliminate_pm_fun env ps body in
          return @@ ((name, new_args, new_expr) :: acc, env))
        (return ([], env))
        cases
    in
    let new_cases = List.rev new_cases in
    return @@ ([ PMFDRecursive (List.hd new_cases, List.tl new_cases) ], env)
;;

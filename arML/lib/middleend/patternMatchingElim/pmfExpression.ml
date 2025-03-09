(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateMonad
open Common.StateMonad.Syntax
open Common.IdentifierStructs
open Ast.AbstractSyntaxTree
open LambdaLifting.Llast
open Pmfast
open ImmutBasicFunctions
open GenerateDeclarationsFromCase
open GenerateConditionsFromCase
open PmfCase

let get_new_pmf_name = Common.NameCreator.get_new_name "pmf"

let rec eliminate_pm_expression env = function
  | LEConstant c -> return @@ (PMFConstant c, env)
  | LEIdentifier id -> return @@ (PMFIdentifier id, env)
  | LEEmptyList -> return @@ (PMFEmptyList, env)
  | LEApplication (func, arg, args) ->
    let* func', env = eliminate_pm_expression env func in
    let* arg', env = eliminate_pm_expression env arg in
    let* args', env = 
      List.fold_right 
        (fun arg acc -> 
           let* acc, env = acc in
           let* new_arg, env = eliminate_pm_expression env arg in
           return (new_arg :: acc, env))
        args 
        (return ([], env))
    in
    return @@ (PMFApplication (func', arg', args'), env)
  | LEIfThenElse (c, b1, b2) ->
    let* c', env = eliminate_pm_expression env c in
    let* b1', env = eliminate_pm_expression env b1 in
    let* b2', env = 
      (match b2 with
       | Some expr ->
         let* b2', env = eliminate_pm_expression env expr in
         return @@ (Some (b2'), env)
       | None -> return (None, env))
    in
    return @@ (PMFIfThenElse (c', b1', b2'), env)
  | LETuple (e1, e2, es) ->
    let* e1', env = eliminate_pm_expression env e1 in
    let* e2', env = eliminate_pm_expression env e2 in
    let* es', env = 
      List.fold_right 
        (fun e acc -> 
           let* acc, env = acc in
           let* new_e, env = eliminate_pm_expression env e in
           return ((new_e :: acc), env))
        es 
        (return ([], env))
    in
    return @@ (PMFTuple (e1', e2', es'), env)
  | LEListConstructor (l, r) ->
    let* l', env = eliminate_pm_expression env l in
    let* r', env = eliminate_pm_expression env r in
    return @@ (PMFListConstructor (l', r'), env)
  | LETyped (e, t) -> 
    let* e', env = eliminate_pm_expression env e in
    return @@ (PMFTyped (e', t), env)
  | LELetIn ((p, e), [], body) ->
    let* e', env = eliminate_pm_expression env e in
    let* body', env = eliminate_pm_expression env body in
    let branches = (body', pattern_matching_failure_flag) in 
    (match e', p with
     | expr, PVar name -> return @@ (PMFLetIn ((name, expr), body'), env)
     | expr, PConst CUnit -> return @@ (PMFLetIn ((Id ("()"), expr), body'), env)
     | PMFIdentifier _, _ -> return @@ (eliminate_pm_case (p, e') branches, env)
     | _ ->
       let* name = get_new_pmf_name env in
       let name = Id name in
       let env = IdentifierSet.add name env in
       let new_expr = eliminate_pm_case (p, (PMFIdentifier name)) branches in
       return @@ ((PMFLetIn ((name, e'), new_expr)), env))
  | LELetIn (case, cases, body) ->
    let rec helper acc = function
      | [] -> acc
      | hd :: tl -> helper (LELetIn (hd, [], acc)) tl
    in
    let new_expr = helper body (case :: cases) in
    eliminate_pm_expression env new_expr
  | LEMatchWith (expr, case, cases) ->
    let rec eliminate_match_with env to_match = function
      | [] -> return @@ (pattern_matching_failure_flag, env)
      | hd :: tl ->
        let p, e = hd in
        let new_decls = generate_declarations_from_case (p, to_match) in
        let* e', env = eliminate_pm_expression env e in
        let e' = List.fold_right (fun d acc -> PMFLetIn (d, acc)) new_decls e' in
        let* tail_e', env = eliminate_match_with env to_match tl in
        let branches = (e', tail_e') in
        return @@ (generate_condition_if_needed (p, to_match) branches, env)
    in
    let* expr', env = eliminate_pm_expression env expr in
    (match expr' with
     | PMFIdentifier _ -> eliminate_match_with env expr' (case :: cases)
     | PMFConstant _  -> eliminate_match_with env expr' (case :: cases)
     | _ -> 
       let* name = get_new_pmf_name env in
       let name = Id name in
       let* pmf_free, env = eliminate_match_with env (PMFIdentifier name) (case :: cases) in
       return @@ (PMFLetIn ((name, expr'), pmf_free), env))
;;

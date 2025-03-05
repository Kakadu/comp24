(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Common.StateMonad
open Common.StateMonad.Syntax
open LlExpression
open Llast

let ll_decl_case env replacement_map lifted = function
  | main_p, EFun ((p1, ps), body) ->
    let* lifted_body, lifted_exprs = ll_expression env replacement_map lifted body in
    return ((main_p, p1 :: ps, lifted_body), lifted_exprs)
  | main_p, expr ->
    let* lifted_expr, lifted_exprs = ll_expression env replacement_map lifted expr in
    return ((main_p, [], lifted_expr), lifted_exprs)
;;

let ll_decl env replacement_map lifted = function
  | DOrdinary (case, cases) ->
    let* (name, args, lifted_case), lifted_exprs1 =
      ll_decl_case env replacement_map lifted case
    in
    let* lifted_cases, lifted_exprs2 =
      List.fold_right
        (fun c acc ->
          let* acc = acc in
          let* (c_name, c_args, lifted_c), lifted_e =
            ll_decl_case env replacement_map lifted c
          in
          return ((c_name, c_args, lifted_c) :: fst acc, lifted_e @ snd acc))
        cases
        (return ([], []))
    in
    return
      (LDOrdinary ((name, args, lifted_case), lifted_cases), lifted_exprs1 @ lifted_exprs2)
  | DRecursive (case, cases) ->
    let* (name, args, lifted_case), lifted_exprs1 =
      ll_decl_case env replacement_map lifted case
    in
    let* lifted_cases, lifted_exprs2 =
      List.fold_right
        (fun c acc ->
          let* acc = acc in
          let* (c_name, c_args, lifted_c), lifted_e =
            ll_decl_case env replacement_map lifted c
          in
          return ((c_name, c_args, lifted_c) :: fst acc, lifted_e @ snd acc))
        cases
        (return ([], []))
    in
    let extract_cases lifted_exprs =
      List.fold_left
        (fun acc expr ->
          match expr with
          | LDOrdinary (case, cases) -> (case :: cases) @ acc
          | LDRecursive (case, cases) -> (case :: cases) @ acc)
        []
        lifted_exprs
    in
    let lifted_cases = lifted_cases @ extract_cases (lifted_exprs1 @ lifted_exprs2) in
    return (LDRecursive ((name, args, lifted_case), lifted_cases), lifted)
;;

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Anftree

let rec anf_to_program anf_program = List.map anf_decl_to_decl anf_program

and anf_decl_to_decl = function
  | ADOrdinary (id, args, body) ->
    let pattern = PVar id in
    let expr =
      if List.length args > 0
      then (
        let args =
          List.map
            (fun a ->
              match a with
              | Id name when name = "()" -> PConst CUnit
              | _ -> PVar a)
            args
        in
        EFun ((List.hd args, List.tl args), anf_expr_to_expr body))
      else anf_expr_to_expr body
    in
    let case = pattern, expr in
    DOrdinary (case, [])
  | ADRecursive (decl, decls) ->
    let decls = decl :: decls in
    let cases =
      List.map
        (fun (id, args, body) ->
          let pattern = PVar id in
          let expr =
            if List.length args > 0
            then (
              let args = List.map (fun a -> PVar a) args in
              EFun ((List.hd args, List.tl args), anf_expr_to_expr body))
            else anf_expr_to_expr body
          in
          pattern, expr)
        decls
    in
    DRecursive (List.hd cases, List.tl cases)

and anf_expr_to_expr = function
  | ALetIn (id, complex_expr, body) ->
    let pattern = PVar id in
    let expr = complex_expr_to_expr complex_expr in
    let case = pattern, expr in
    ELetIn (case, [], anf_expr_to_expr body)
  | AComplex complex_expr -> complex_expr_to_expr complex_expr

and complex_expr_to_expr = function
  | CAtom immut_expr -> immut_expr_to_expr immut_expr
  | CApplication (func, arg, args) ->
    let func_expr = immut_expr_to_expr func in
    let arg_expr = immut_expr_to_expr arg in
    let args_exprs = List.map immut_expr_to_expr args in
    EApplication (func_expr, arg_expr, args_exprs)
  | CIfThenElse (cond, then_branch, else_branch) ->
    let cond_expr = immut_expr_to_expr cond in
    let then_expr = anf_expr_to_expr then_branch in
    let else_expr = anf_expr_to_expr else_branch in
    EIfThenElse (cond_expr, then_expr, Some else_expr)
  | CListConstructor (head, tail) ->
    let head_expr = immut_expr_to_expr head in
    let tail_expr = immut_expr_to_expr tail in
    EListConstructor (head_expr, tail_expr)
  | CTyped (e, t) ->
    let e' = complex_expr_to_expr e in
    ETyped (e', t)

and immut_expr_to_expr = function
  | IConstant c -> EConstant c
  | IEmptyList -> EEmptyList
  | IIdentifier (Id name) when name = "()" -> EConstant CUnit
  | IIdentifier id -> EIdentifier id
  | ITuple (e1, e2, es) ->
    let e1_expr = immut_expr_to_expr e1 in
    let e2_expr = immut_expr_to_expr e2 in
    let es_exprs = List.map immut_expr_to_expr es in
    ETuple (e1_expr, e2_expr, es_exprs)
;;

(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Base
open Utils
open Vars

(** Remove trivial applications and merge nested lambdas *)
let simplify =
  let rec expr = function
    | (EConst _ as e) | (EVar _ as e) -> e
    | EApp (l, r) -> e_app (expr l) (expr r)
    | EIfElse (i, t, e) -> e_if_else (expr i) (expr t) (expr e)
    | EFun ((PIdent x :: _ as args), EApp (inner, EVar y)) when String.equal x y ->
      let inner' = expr inner in
      (match inner' with
       | EFun _ -> inner'
       | _ -> EFun (args, EApp (inner', EVar y)))
    | EFun (args1, EFun (args2, body)) ->
      e_fun (List.append args1 args2) (expr body) |> expr
    | EFun (p, e) -> e_fun p (expr e)
    | ELetIn (DLet (r, p, e1), e2) ->
      e_let_in (d_let_flag r p (expr e1)) (expr e2)
    | ETuple xs -> e_tuple (List.map xs ~f:expr)
    | EList xs -> e_list (List.map xs ~f:expr)
    | EMatch (e, c) ->
      e_match (expr e) (List.map c ~f:(fun (p, e) -> p, expr e))
  in
  let def = function
    | DLet (r, p, e) -> d_let_flag r p (expr e)
  in
  List.map ~f:def
;;

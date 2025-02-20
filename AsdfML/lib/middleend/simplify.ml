(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

(* open Tast *)
open Sast
open Types
open Base
open Utils
open Vars

(** Remove trivial applications and merge nested lambdas *)
let simplify =
  let rec expr = function
    | (SConst _ as e) | (SVar _ as e) -> e
    | SApp (l, r) -> s_app (expr l) (expr r)
    | SIfElse (i, t, e) -> s_if_else (expr i) (expr t) (expr e)
    | SFun (arg1, args1, (SApp (inner, SVar arg2) as apply)) when String.equal arg1 arg2
      ->
      let inner' = expr inner in
      (match inner' with
       | SFun _ -> expr inner'
       | _ -> SFun (arg1, args1, apply))
    | SFun (arg1, args1, SFun (arg2, args2, body)) ->
      s_fun arg1 (args1 @ [ arg2 ] @ args2) (expr body) |> expr
    | SFun (p, ps, e) -> s_fun p ps (expr e)
    | SLetIn (d, e2) -> s_let_in (def d) (expr e2)
    | STuple (x1, x2, xs) -> s_tuple (expr x1) (expr x2) (List.map xs ~f:expr)
    | SList xs -> s_list (List.map xs ~f:expr)
  and def = function
    | SLet (r, p, e) -> s_let_flag r p (expr e)
  in
  List.map ~f:def
;;

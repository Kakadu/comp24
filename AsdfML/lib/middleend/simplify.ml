(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Tast
open Types
open Base
open Utils
open Vars

(** Remove trivial applications and merge nested lambdas *)
let simplify =
  let rec expr = function
    | (TEConst _ as e) | (TEVar _ as e) -> e
    | TEApp (t, l, r) -> te_app t (expr l) (expr r)
    | TEIfElse (ty, i, t, e) -> te_if_else ty (expr i) (expr t) (expr e)
    | TEFun (tf, (PIdent x as arg), args, (TEApp (_, inner, TEVar (_, y)) as apply))
      when String.equal x y ->
      let inner' = expr inner in
      (match inner' with
       | TEFun _ -> expr inner'
       | _ -> TEFun (tf, arg, args, apply))
    | TEFun (t1, arg1, args1, TEFun (t2, arg2, args2, body)) ->
      let rec helper = function
        | TArrow (t1, t2) -> t1 :: helper t2
        | _ -> []
      in
      let t = List.fold_right (helper t1) ~init:t2 ~f:(fun t1 t2 -> t1 ^-> t2) in
      te_fun t arg1 (args1 @ [ arg2 ] @ args2) (expr body) |> expr
    | TEFun (t, p, ps, e) -> te_fun t p ps (expr e)
    | TELetIn (t, d, e2) -> te_let_in t (def d) (expr e2)
    | TETuple (t, x1, x2, xs) -> te_tuple t (expr x1) (expr x2) (List.map xs ~f:expr)
    | TEList (t, xs) -> te_list t (List.map xs ~f:expr)
    | TEMatch (t, e, c) -> te_match t (expr e) (List.map c ~f:(fun (p, e) -> p, expr e))
  and def = function
    | TDLet (t, r, p, e) -> td_let_flag r t p (expr e)
  in
  List.map ~f:def
;;

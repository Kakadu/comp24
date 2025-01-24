(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let true_const = Expr_const (Const_bool true)

let rec match_condition p e =
  match p, e with
  | Pat_var _, _ | Pat_wildcard, _ -> true_const
  | Pat_constrained (p, _), _ -> match_condition p e
  | Pat_const c, e -> eqq e (Expr_const c)
  | Pat_cons (px, pxs), Expr_cons (ex, exs) ->
    eland (match_condition px ex) (match_condition pxs exs)
  | Pat_cons (px, pxs), e ->
    let not_nil = neq e enil in
    let head = getfield 0 e in
    let head_cond = match_condition px head in
    let tail = getfield 1 e in
    let tail_cond = match_condition pxs tail in
    eland (eland not_nil head_cond) tail_cond
  | Pat_tuple (pfst, psnd, prest), Expr_tuple (efst, esnd, erest) ->
    match_list (pfst :: psnd :: prest) (efst :: esnd :: erest)
  | Pat_tuple (pfst, psnd, prest), e ->
    let ps = pfst :: psnd :: prest in
    let len = List.length ps in
    let es = Base.List.range ~stop:`exclusive 0 len |> List.map (fun i -> getfield i e) in
    match_list ps es

and match_list ps es =
  List.fold_left2 (fun acc p e -> eland acc (match_condition p e)) true_const ps es
;;

let rec eliminate_expr = function
  | (Expr_const _ | Expr_var _) as c -> c
  | Expr_cons (x, xs) -> Expr_cons (eliminate_expr x, eliminate_expr xs)
  | Expr_tuple (fst, snd, rest) ->
    let fst' = eliminate_expr fst in
    let snd' = eliminate_expr snd in
    let rest' = List.map eliminate_expr rest in
    Expr_tuple (fst', snd', rest')
  | Expr_ite (c, t, e) ->
    eite_simplified (eliminate_expr c) (eliminate_expr t) (eliminate_expr e)
  | Expr_app (f, a) -> Expr_app (eliminate_expr f, eliminate_expr a)
  | Expr_constrained (e, t) -> Expr_constrained (eliminate_expr e, t)
  | Expr_let (rf, (p, e), scope) ->
    Expr_let (rf, (p, eliminate_expr e), eliminate_expr scope)
  | Expr_fun (p, e) -> Expr_fun (p, eliminate_expr e)
  | Expr_match (e, cases) ->
    let conds = List.map (fun (p, _) -> match_condition p e) cases in
    let base = Runtime.partial_match_error e in
    List.fold_right2 (fun mc (_, e) acc -> eite_simplified mc e acc) conds cases base
;;

let eliminate structure =
  let eliminate_item (Str_value (rf, bindings)) =
    let bindings = List.map (fun (p, e) -> p, eliminate_expr e) bindings in
    Str_value (rf, bindings)
  in
  List.map eliminate_item structure
;;

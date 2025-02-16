(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Ast
open Tast

let vars_pat, vars_pat_list =
  let open Set.Poly in
  let rec helper = function
    | PConst _ | PWild -> empty
    | PIdent x -> singleton x
    | PTuple (hd1, hd2, tl) ->
      let xs = hd1 :: hd2 :: tl in
      union_list (List.map xs ~f:helper)
    | PList xs -> union_list (List.map xs ~f:helper)
    | PCons (l, r) -> union (helper l) (helper r)
    | PAnn (x, _) -> helper x
  in
  helper, Fn.compose union_list (List.map ~f:helper)
;;

let rec free_vars_expr =
  let open Set.Poly in
  function
  | EConst _ -> empty
  | EVar id -> singleton id
  | EApp (l, r) -> union (free_vars_expr l) (free_vars_expr r)
  | EIfElse (c, t, e) ->
    union_list [ free_vars_expr c; free_vars_expr t; free_vars_expr e ]
  | EFun (pat, exp) -> diff (free_vars_expr exp) (vars_pat_list pat)
  | ELetIn ((DLet (_, pat, _) as def), exp) ->
    let free_def = free_vars_def def in
    let free_expr = diff (free_vars_expr exp) (vars_pat pat) in
    union free_def free_expr
  | ETuple (hd1, hd2, tl) ->
    let xs = hd1 :: hd2 :: tl in
    union_list (List.map xs ~f:free_vars_expr)
  | EList xs -> union_list (List.map xs ~f:free_vars_expr)
  | EMatch (e, pe) ->
    let free_cases =
      List.fold pe ~init:empty ~f:(fun acc (p, e) ->
        union acc (diff (free_vars_expr e) (vars_pat p)))
    in
    union (free_vars_expr e) free_cases

and free_vars_def =
  let open Set.Poly in
  function
  | DLet (NonRec, _, exp) -> free_vars_expr exp
  | DLet (Rec, pat, exp) -> diff (free_vars_expr exp) (vars_pat pat)
;;

let free_vars_texpr e = strip_types_expr e |> free_vars_expr
let free_vars_tdef d = strip_types_def d |> free_vars_def

let rec vars_expr =
  let open Set.Poly in
  function
  | EConst _ -> empty
  | EVar id -> singleton id
  | EApp (l, r) -> union (vars_expr l) (vars_expr r)
  | EIfElse (c, t, e) -> union_list [ vars_expr c; vars_expr t; vars_expr e ]
  | EFun (pat, exp) -> union (vars_pat_list pat) (vars_expr exp)
  | ELetIn ((DLet _ as def), exp) -> union (vars_def def) (vars_expr exp)
  | ETuple (hd1, hd2, tl) ->
    let xs = hd1 :: hd2 :: tl in
    union_list (List.map xs ~f:vars_expr)
  | EList xs -> union_list (List.map xs ~f:vars_expr)
  | EMatch (e, pe) ->
    let vars_cases =
      List.fold pe ~init:empty ~f:(fun acc (p, e) ->
        union_list [ acc; vars_expr e; vars_pat p ])
    in
    union (vars_expr e) vars_cases

and vars_def =
  let open Set.Poly in
  function
  | DLet (_, pat, exp) -> union (vars_expr exp) (vars_pat pat)
;;

let vars_texpr e = strip_types_expr e |> vars_expr

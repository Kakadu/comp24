(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Ast
open Tast

let bound_vars_pat, bound_vars_pat_list =
  let open Set.Poly in
  let rec helper = function
    | PConst _ | PWild -> empty
    | PIdent x -> singleton x
    | PTuple xs | PList xs -> union_list (List.map xs ~f:helper)
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
  | EFun (pat, exp) -> diff (free_vars_expr exp) (bound_vars_pat_list pat)
  | ELetIn ((DLet (_, pat, _) as def), exp) ->
    let free_def = free_vars_def def in
    let free_expr = diff (free_vars_expr exp) (bound_vars_pat pat) in
    union free_def free_expr
  | ETuple xs | EList xs -> union_list (List.map xs ~f:free_vars_expr)
  | EMatch (e, pe) ->
    let free_cases =
      List.fold pe ~init:empty ~f:(fun acc (p, e) ->
        union acc (diff (free_vars_expr e) (bound_vars_pat p)))
    in
    union (free_vars_expr e) free_cases

and free_vars_def =
  let open Set.Poly in
  function
  | DLet (NonRec, _, exp) -> free_vars_expr exp
  | DLet (Rec, pat, exp) -> diff (free_vars_expr exp) (bound_vars_pat pat)
;;

let free_vars_texpr e = strip_types_expr e |> free_vars_expr
let free_vars_tdef d = strip_types_def d |> free_vars_def

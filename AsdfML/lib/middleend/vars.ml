(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Ast
open Sast

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

let rec vars_expr =
  let open Set.Poly in
  function
  | EConst _ -> empty
  | EVar id -> singleton id
  | EApp (l, r) -> union (vars_expr l) (vars_expr r)
  | EIfElse (c, t, e) -> union_list [ vars_expr c; vars_expr t; vars_expr e ]
  | EFun (p, ps, exp) -> union (vars_pat_list (p :: ps)) (vars_expr exp)
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

let rec free_vars_sexpr =
  let open Set.Poly in
  function
  | SConst _ -> empty
  | SVar id -> singleton id
  | SApp (l, r) -> union (free_vars_sexpr l) (free_vars_sexpr r)
  | SIfElse (c, t, e) ->
    union_list [ free_vars_sexpr c; free_vars_sexpr t; free_vars_sexpr e ]
  | SFun (p, ps, exp) -> diff (free_vars_sexpr exp) (of_list (p :: ps))
  | SLetIn ((SLet (_, id, _) as def), exp) ->
    let free_def = free_vars_sdef def in
    let free_expr = diff (free_vars_sexpr exp) (singleton id) in
    union free_def free_expr
  | STuple (hd1, hd2, tl) ->
    let xs = hd1 :: hd2 :: tl in
    union_list (List.map xs ~f:free_vars_sexpr)
  | SList xs -> union_list (List.map xs ~f:free_vars_sexpr)

and free_vars_sdef =
  let open Set.Poly in
  function
  | SLet (NonRec, _, exp) -> free_vars_sexpr exp
  | SLet (Rec, id, exp) -> diff (free_vars_sexpr exp) (singleton id)
;;

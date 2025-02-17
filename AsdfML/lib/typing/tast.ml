(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Types
open Pp_ast
open Pp_typing

type texpr =
  | TEConst of ty * Ast.constant
  | TEVar of ty * Ast.id
  | TEApp of ty * texpr * texpr
  | TEIfElse of ty * texpr * texpr * texpr
  | TEFun of ty * pattern list * texpr
  | TELetIn of ty * tdefinition * texpr
  | TETuple of ty * texpr * texpr * texpr list
  | TEList of ty * texpr list
  | TEMatch of ty * texpr * (pattern * texpr) list
[@@deriving show { with_path = false }, eq]

and tdefinition = TDLet of ty * Ast.rec_flag * pattern * texpr
[@@deriving show { with_path = false }]

type tprogram = tdefinition list [@@deriving show { with_path = false }, eq]

let te_const t c = TEConst (t, c)
let te_var t x = TEVar (t, x)
let te_app t f x = TEApp (t, f, x)
let te_if_else t cond e_true e_false = TEIfElse (t, cond, e_true, e_false)
let te_fun t p e = TEFun (t, p, e)
let te_let_in t def e = TELetIn (t, def, e)
let te_tuple t e1 e2 es = TETuple (t, e1, e2, es)
let te_list t exprs = TEList (t, exprs)
let te_match t e branches = TEMatch (t, e, branches)
let td_let t p e = TDLet (t, NonRec, p, e)
let td_let_rec t p e = TDLet (t, Rec, p, e)

let td_let_flag = function
  | Rec -> td_let_rec
  | NonRec -> td_let
;;

open Base

let texpr_type = function
  | TEConst (t, _)
  | TEVar (t, _)
  | TEApp (t, _, _)
  | TEIfElse (t, _, _, _)
  | TEFun (t, _, _)
  | TELetIn (t, _, _)
  | TETuple (t, _, _, _)
  | TEList (t, _)
  | TEMatch (t, _, _) -> t
;;

let rec strip_types_expr = function
  | TEConst (_, c) -> EConst c
  | TEVar (_, x) -> EVar x
  | TEApp (_, f, x) -> EApp (strip_types_expr f, strip_types_expr x)
  | TEIfElse (_, c, t, e) ->
    EIfElse (strip_types_expr c, strip_types_expr t, strip_types_expr e)
  | TEFun (_, p, e) -> EFun (p, strip_types_expr e)
  | TELetIn (_, d, e) -> ELetIn (strip_types_def d, strip_types_expr e)
  | TETuple (_, e1, e2, es) ->
    ETuple (strip_types_expr e1, strip_types_expr e2, List.map es ~f:strip_types_expr)
  | TEList (_, es) -> EList (List.map es ~f:strip_types_expr)
  | TEMatch (_, e, pes) ->
    EMatch (strip_types_expr e, List.map ~f:(fun (p, e) -> p, strip_types_expr e) pes)

and strip_types_def = function
  | TDLet (_, flag, p, e) -> DLet (flag, p, strip_types_expr e)
;;

let pp_texpr fmt e = strip_types_expr e |> Pp_ast.pp_expr fmt
and pp_tdefinition fmt d = strip_types_def d |> Pp_ast.pp_definition fmt

let pp_tprogram fmt p = List.map p ~f:strip_types_def |> Pp_ast.pp_program fmt

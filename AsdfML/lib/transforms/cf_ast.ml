(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

type cf_expr =
  | CFConst of constant
  | CFVar of id
  | CFApp of cf_expr * cf_expr
  | CFIfElse of cf_expr * cf_expr * cf_expr
  | CFFun of id list * cf_expr
  | CFLetIn of id * cf_expr * cf_expr (* TODO: <- *)
  | CFTuple of cf_expr list
  | CFList of cf_expr list
  | CFMatch of cf_expr * (pattern * cf_expr) list

and cf_definition = CFDLet of id * cf_expr

type program = cf_definition list

let cf_const c = CFConst c
let cf_var v = CFVar v
let cf_app f arg = CFApp (f, arg)
let cf_if_else cond if_expr else_expr = CFIfElse (cond, if_expr, else_expr)
let cf_fun args body = CFFun (args, body)
let cf_let_in v expr body = CFLetIn (v, expr, body)
let cf_tuple exprs = CFTuple exprs
let cf_list exprs = CFList exprs
let cf_match expr cases = CFMatch (expr, cases)
let cf_def id expr = CFDLet (id, expr)

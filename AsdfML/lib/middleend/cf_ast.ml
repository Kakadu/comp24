(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

type cf_expr =
  | CFConst of constant
  | CFVar of id
  | CFApp of cf_expr * cf_expr
  | CFIfElse of cf_expr * cf_expr * cf_expr
  | CFLetIn of id * cf_expr * cf_expr
  | CFTuple of cf_expr * cf_expr * cf_expr list
  | CFList of cf_expr list

and cf_definition = CFLet of id * id list * cf_expr

type program = cf_definition list

let cf_const c = CFConst c
let cf_var v = CFVar v
let cf_app f arg = CFApp (f, arg)
let cf_if_else cond if_expr else_expr = CFIfElse (cond, if_expr, else_expr)
let cf_let_in id body exp = CFLetIn (id, body, exp)
let cf_tuple e1 e2 es = CFTuple (e1, e2, es)
let cf_list exprs = CFList exprs
let cf_def id args expr = CFLet (id, args, expr)

open Base
open Format
open Utils

let rec pp_expr fmt = function
  | CFConst c -> fprintf fmt "%a" Pp_ast.pp_constant c
  | CFVar v -> fprintf fmt "%s" v
  | CFApp (e1, e2) ->
    let rec pp_rest fmt = function
      | CFApp (e1, e2) -> fprintf fmt "%a %a" pp_rest e1 pp_expr e2
      | e -> fprintf fmt "(%a" pp_expr e
    in
    fprintf fmt "%a %a)" pp_rest e1 pp_expr e2
  | CFIfElse (c, t, e) ->
    fprintf
      fmt
      "if %a @\n@[<2>then@ %a@] @\n@[<2>else@ %a@]"
      pp_expr
      c
      pp_expr
      t
      pp_expr
      e
  | CFLetIn (id, body, exp) ->
    fprintf fmt "@[<2>@,let %s =@ %a @]in@\n%a" id pp_expr body pp_expr exp
  | CFTuple (x, y, xs) ->
    let xs = x :: y :: xs in
    pp_list ~sep:", " fmt pp_expr xs
  | CFList xs -> pp_list ~op:"[" ~cl:"]" ~sep:"; " fmt pp_expr xs

and pp_definition fmt = function
  | CFLet (id, args, e) ->
    (match args with
     | [] -> fprintf fmt "@[<2>@,let %s =@ %a@]" id pp_expr e
     | _ ->
       fprintf fmt "@[<2>@,let %s %s =@ %a@]" id (String.concat args ~sep:" ") pp_expr e)
;;

let pp_program fmt p = List.iter p ~f:(fun d -> fprintf fmt "%a@." pp_definition d)

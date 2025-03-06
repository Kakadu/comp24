(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Reduced_ast
open Parser.Ast

let rec rexpr_to_expr = function
  | RExp_ident i -> Exp_ident i
  | RExp_constant c -> Exp_constant c
  | RExp_let (name, v, i) ->
    Exp_let (Nonrecursive, [ Val_binding (name, [], rexpr_to_expr v) ], rexpr_to_expr i)
  | RExp_let_rec (bindings, exp) ->
    let bindings =
      List.map (fun (s, exp) -> Val_binding (s, [], rexpr_to_expr exp)) bindings
    in
    Exp_let (Recursive, bindings, rexpr_to_expr exp)
  | RExp_fun (args, exp) ->
    let args = List.map (fun arg -> Pat_var arg) args in
    Exp_fun (args, rexpr_to_expr exp)
  | RExp_apply (l, r) -> Exp_apply (rexpr_to_expr l, rexpr_to_expr r)
  | RExp_tuple m -> Exp_tuple (List.map rexpr_to_expr m)
  | RExp_construct (name, v) -> Exp_construct (name, Option.map rexpr_to_expr v)
  | RExp_if (i, t, e) ->
    Exp_if (rexpr_to_expr i, rexpr_to_expr t, Option.map rexpr_to_expr e)
;;

let rstruct_to_struct_item = function
  | RStr_eval e -> Str_eval (rexpr_to_expr e)
  | RStr_value (n, v) -> Str_value (Nonrecursive, [ Val_binding (n, [], rexpr_to_expr v) ])
  | RStr_value_rec vals ->
    Str_value
      ( Recursive
      , List.map (fun (name, exp) -> Val_binding (name, [], rexpr_to_expr exp)) vals )
;;

let rast_to_ast = List.map rstruct_to_struct_item

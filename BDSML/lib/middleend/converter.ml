(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Reduced_ast
open Parser.Ast
open Anf_ast

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

let find_in_ops name =
  match
    List.find_opt (fun (_, _, n) -> name = n) Utils.Predefined_ops.predefine_operators
  with
  | Some (n, _, _) -> n
  | None -> name
;;

let rec aexpr_to_ast = function
  | AExp_ident s -> Exp_ident (find_in_ops s)
  | AExp_constant c -> Exp_constant c
  | AExp_tuple l -> Exp_tuple (List.map aexpr_to_ast l)
  | AExp_construct (name, exp) -> Exp_construct (name, Option.map aexpr_to_ast exp)
;;

let rec cexpr_to_ast = function
  | CExp_if (i, t, e) -> Exp_if (aexpr_to_ast i, lexpr_to_ast t, Some (lexpr_to_ast e))
  | CExp_apply (f, args) ->
    List.fold_left
      (fun f arg -> Exp_apply (f, aexpr_to_ast arg))
      (Exp_ident (find_in_ops f))
      args
  | CExp_atom e -> aexpr_to_ast e

and lexpr_to_ast = function
  | LLet_in (name, v, exp) ->
    Exp_let (Nonrecursive, [ Val_binding (name, [], cexpr_to_ast v) ], lexpr_to_ast exp)
  | LComplex exp -> cexpr_to_ast exp
;;

let absexpr_to_ast = function
  | AbsStr_eval e -> Str_eval (lexpr_to_ast e)
  | AbsStr_func (name, args, exp) ->
    Str_value
      ( Nonrecursive
      , [ Val_binding (name, List.map (fun el -> Pat_var el) args, lexpr_to_ast exp) ] )
  | AbsStr_value (name, exp) ->
    Str_value (Nonrecursive, [ Val_binding (name, [], lexpr_to_ast exp) ])
  | AbsStr_value_rec l ->
    Str_value
      ( Recursive
      , List.map
          (fun (name, args, exp) ->
            Val_binding (name, List.map (fun el -> Pat_var el) args, lexpr_to_ast exp))
          l )
;;

let anf_to_ast : anf -> structure = List.map absexpr_to_ast

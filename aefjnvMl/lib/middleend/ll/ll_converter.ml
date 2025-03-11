(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Match_elimination.Me_ast
open Ll_ast

let convert_ll_ident = function
  | Id_unit -> Pat_const Const_unit
  | Id_name s -> Pat_var s
;;

let rec convert_ll_expr = function
  | LL_const c -> Exp_constant c
  | LL_ident name -> Exp_ident name
  | LL_ifthenelse (i, t, e) ->
    Exp_ifthenelse (convert_ll_expr i, convert_ll_expr t, convert_ll_expr e)
  | LL_tuple e'l ->
    let converted = List.map convert_ll_expr e'l in
    Exp_tuple converted
  | LL_list (eh, etl) -> Exp_list (convert_ll_expr eh, convert_ll_expr etl)
  | LL_apply (e1, e2) -> Exp_apply (convert_ll_expr e1, convert_ll_expr e2)
  | LL_let (id, e, cont) ->
    let decl = decl_var id e in
    Exp_let (decl, convert_ll_expr cont)

and decl_var id e =
  Decl (Nonrecursive, [ { vb_pat = convert_ll_ident id; vb_expr = convert_ll_expr e } ])
;;

let convert_func f =
  let convert_args args =
    List.fold_left
      (fun acc arg_id next ->
        let arg_id' = convert_ll_ident arg_id in
        acc @@ Exp_function (arg_id', next))
      (fun x -> x)
      args
  in
  let vb_pat = Pat_var f.lldec_name in
  let vb_expr =
    let arg1, args = f.lldec_args in
    let e' = convert_ll_expr f.lldec_body in
    convert_args (arg1 :: args) e'
  in
  { vb_pat; vb_expr }
;;

let convert_ll_structure_item str =
  let decl =
    match str with
    | LL_GlobalV (id, e) -> decl_var id e
    | LL_Decl (r_flag, f1, f'l) ->
      let vb'l = List.map convert_func (f1 :: f'l) in
      Decl (r_flag, vb'l)
  in
  Str_value decl
;;

let convert_ll_program prog = List.map convert_ll_structure_item prog

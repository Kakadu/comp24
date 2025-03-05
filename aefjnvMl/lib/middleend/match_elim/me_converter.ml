(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Me_ast

let convert_me_ident = function
  | Unit -> Pat_const Const_unit
  | Name s -> Pat_var s
;;

let rec convert_expr = function
  | MExp_constant c -> Exp_constant c
  | MExp_ident i -> Exp_ident i
  | MExp_tuple es ->
    let converted = List.map convert_expr es in
    Exp_tuple converted
  | MExp_apply (e1, e2) -> Exp_apply (convert_expr e1, convert_expr e2)
  | MExp_list (e1, e2) -> Exp_list (convert_expr e1, convert_expr e2)
  | MExp_ifthenelse (e1, e2, e3) ->
    Exp_ifthenelse (convert_expr e1, convert_expr e2, convert_expr e3)
  | MExp_function (param, body) -> Exp_function (convert_me_ident param, convert_expr body)
  | MExp_let (decl, body) -> Exp_let (convert_decl decl, convert_expr body)

and convert_value_binding vb =
  { vb_pat = convert_me_ident vb.m_vb_pat; vb_expr = convert_expr vb.m_vb_expr }

and convert_decl (MDecl (rec_flag, bindings)) =
  Decl (rec_flag, List.map convert_value_binding bindings)
;;

let convert_program decls = List.map (fun decl -> Str_value (convert_decl decl)) decls

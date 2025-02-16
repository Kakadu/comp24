(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let cint n = Const_int n
let cbool b = Const_bool b
let cnil = Const_nil
let cunit = Const_unit

(*===================== core_type =====================*)

let ptint = Ptyp_int
let ptbool = Ptyp_bool
let ptunit = Ptyp_unit
let ptlist tp = Ptyp_list tp
let ptvar idnt = Ptyp_var idnt
let pttuple pt_list = Ptyp_tuple pt_list
let ptarrow before_pt after_pt = Ptyp_arrow (before_pt, after_pt)

(*===================== pattern =====================*)

let pconst c = Pat_const c
let pvar c = Pat_var c
let pcons a b = Pat_cons (a, b)
let pany = Pat_any
let ptuple ps = Pat_tuple ps
let pconstraint p ct = Pat_constraint (p, ct)

(*===================== expression =====================*)

let etype e tp = Exp_type (e, tp)
let econst c = Exp_constant c
let eval c = Exp_ident c
let eapp f a = Exp_apply (f, a)
let ematch v ptrns = Exp_match (v, ptrns)
let efun i e = Exp_function (i, e)
let ebinop o l r = Exp_apply (Exp_apply (o, l), r)
let eunop o e = Exp_apply (o, e)
let eite b t e = Exp_ifthenelse (b, t, e)
let elet d e = Exp_let (d, e)
let etuple es = Exp_tuple es
let econs a b = Exp_list (a, b)
let evalue_binding vb_pat vb_expr = { vb_pat; vb_expr }
let edecl rec_flag bind_list = Decl (rec_flag, bind_list)
let streval e = Str_eval e
let strval d = Str_value d

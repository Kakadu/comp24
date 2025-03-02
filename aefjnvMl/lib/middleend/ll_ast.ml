(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** TODO (mutual rec):
    - mutaual recursion can be only with [rec] (invariant)
      --> [and] can be only with functions
      --> mutaual recursion can be only in ll_decl as global funcs (ll-guarantees) *)

open Match_elimination.Me_ast

type ll_expr =
  | LL_const of Common.Ast.const
  | LL_ident of Common.Ast.ident
  | LL_tuple of ll_expr list
  | LL_apply of ll_expr * ll_expr
  | LL_list of ll_expr * ll_expr
  | LL_ifthenelse of ll_expr * ll_expr * ll_expr
  | LL_let of ll_bind * ll_expr

and ll_bind = me_ident * ll_expr

type ll_global_var = me_ident * ll_expr

type ll_fun =
  { lldec_rf : Common.Ast.rec_flag
  ; lldec_name : me_ident
  ; lldec_args : me_ident * me_ident list
  ; lldec_body : ll_expr
  }

type ll_structure_item =
  | LLGlobalV of ll_global_var
  | LLDecl of ll_fun * ll_fun list (** if mutual rec *)

type ll_program = ll_structure_item list

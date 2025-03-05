(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** TODO (mutual rec):
    - mutaual recursion can be only with [rec] (invariant)
      --> [and] can be only with functions
      --> mutaual recursion can be only in ll_decl as global funcs (ll-guarantees) *)

open Match_elimination.Me_ast

type ll_expr =
  | LL_const of Common.Ast.const
  | LL_ident of me_ident
  | LL_let of me_ident * ll_expr * ll_expr
  | LL_ifthenelse of ll_expr * ll_expr * ll_expr
  | LL_tuple of ll_expr list
  | LL_list of ll_expr * ll_expr
  (*  *)
  | LL_apply of ll_expr * ll_expr

type ll_fun =
  { lldec_name : string
  ; lldec_args : me_ident * me_ident list
  ; lldec_body : ll_expr
  }

type ll_structure_item =
  | LL_GlobalV of me_ident * ll_expr
  | LL_Decl of Common.Ast.rec_flag * ll_fun * ll_fun list (** if mutual rec *)

type ll_program = ll_structure_item list

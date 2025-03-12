(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val eliminate_match_in_program
  :  Common.Ast.structure_item list
  -> (Me_ast.m_decl list, Common.Errors.error) result

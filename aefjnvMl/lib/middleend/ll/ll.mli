(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val lift_lambdas
  :  Match_elimination.Me_ast.m_decl list
  -> (Ll_ast.ll_structure_item list, Common.Errors.error) result

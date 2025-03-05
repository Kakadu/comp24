(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val convert_ll_structure_item :
  Ll_ast.ll_structure_item -> Common.Ast.structure_item
val convert_ll_program :
  Ll_ast.ll_structure_item list -> Common.Ast.structure_item list

(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val convert_to_anf
  :  Ll_conversion.Ll_ast.ll_structure_item list
  -> (Anf_ast.anf_decl list, Common.Errors.error) result

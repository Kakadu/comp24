(** Copyright 2025, tepa46 *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

val id_from_string : string -> (Ast.identifier, string) result
val const_from_string : string -> (Ast.constant, string) result
val type_from_string : string -> (Ast.typ, string) result
val pattern_from_string : string -> (Ast.pattern, string) result
val expr_from_string : string -> (Ast.expr, string) result
val structure_item_from_string : string -> (Ast.structure_item, string) result
val structure_from_string : string -> (Ast.structure, string) result

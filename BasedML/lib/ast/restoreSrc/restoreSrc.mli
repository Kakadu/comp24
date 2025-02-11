(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

val frestore_expr : Format.formatter -> Ast.expr -> unit
val restore_declarations : Ast.let_declaration list -> string

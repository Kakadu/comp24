(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

val convert_ast : Ast.let_declaration list -> Ast.let_declaration list
val test_closure_convert : Ast.let_declaration list -> unit

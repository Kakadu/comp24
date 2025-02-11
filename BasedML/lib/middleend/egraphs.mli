(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

val simplify : Ast.let_declaration list -> Ast.let_declaration list Ast_mapper.Result.t
val test_egraph_optimization : string -> unit

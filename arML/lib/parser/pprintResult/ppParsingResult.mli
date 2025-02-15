(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Prints the parsed abstract syntax tree (AST) using the formatter. *)
val print_expression_parsing_result : Ast.AbstractSyntaxTree.expression -> unit

val print_program_parsing_result : Ast.AbstractSyntaxTree.program -> unit

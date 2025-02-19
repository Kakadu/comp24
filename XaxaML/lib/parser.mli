(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(** creates AST from text of program *)
val run_parser_program : string -> (Ast.program, string) result

(** creates AST from expression represented by a string *)
val run_parser_expr : string -> (Ast.expr, string) result

(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)
val parse_program : string -> (Ast.let_declaration list, string) result

val test_parse : string -> unit

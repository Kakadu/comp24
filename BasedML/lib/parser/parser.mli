(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

val is_keyword : string -> bool
val p_exp : Ast.expr Angstrom.t
val parse : 'a Angstrom.t -> string -> ('a, string) result
val parse_program : string -> (Ast.let_declaration list, string) result
val test_parse : string -> unit

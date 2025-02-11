(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

val anf_decl_to_string : Anf_ast.anf_decl -> string
val program_to_string : Anf_ast.anf_decl list -> string

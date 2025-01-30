(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

val transform : Llast.lllet_declaration list -> Anf_ast.anf_decl list

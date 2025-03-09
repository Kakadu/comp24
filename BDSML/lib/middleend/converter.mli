(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

val rast_to_ast : Reduced_ast.rstuct_item list -> Parser.Ast.structure
val anf_to_ast : Anf_ast.anf -> Parser.Ast.structure

(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

val rast_to_anf : Reduced_ast.rstruct -> (Anf_ast.anf, Reduced_ast.error) result

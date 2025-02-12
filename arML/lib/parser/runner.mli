(* Copyright 2024-2025, raf-nr and ksenmel *)
(* SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Main parsing functions *)
val parse_program : string -> (Ast.declaration list, string) result
val parse_expression : string -> (Ast.expression list, string) result

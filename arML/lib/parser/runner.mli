(* Copyright 2024-2025, raf-nr and ksenmel *)
(* SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree

(* Main parsing functions *)
val parse_program : string -> (declaration list, string) result
val parse_expression : string -> (expression list, string) result

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree

(* Main expression parser *)
val parse_expression : expression Angstrom.t

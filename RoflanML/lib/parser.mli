(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val parse_expr : string -> (expr, string) result
val parse : string -> (expr list, string) result

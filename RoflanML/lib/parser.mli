(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val parse_expr : id -> (expr, id) result
val parse_decl : id -> (decl, id) result
val parse : id -> (decl list, id) result
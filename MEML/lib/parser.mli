(** Copyright 2024-2025, Perevalov Efim, Dyachkov Vitaliy *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom

val start_parsing : 'a t -> string -> ('a, string) result
val parse_pattern : Ast.pattern t
val parse_expression : Ast.expression t
val parse_bindings : Ast.bindings t
val parse : string -> consume:Consume.t -> (Ast.bindings list, string) result

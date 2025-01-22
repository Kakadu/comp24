(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf
open Ast

val pp_const : Format.formatter -> const -> unit
val binop_to_string : binop -> string
val pp : Format.formatter -> astructure -> unit

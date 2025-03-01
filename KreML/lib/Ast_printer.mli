(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val pp_typ : Format.formatter -> typ -> unit
val pp_expr : Format.formatter -> expr -> unit
val pp_structure : Format.formatter -> structure -> unit
val pattern_to_code : pattern -> string
val expr_to_code : expr -> string
val structure_to_code : structure -> string

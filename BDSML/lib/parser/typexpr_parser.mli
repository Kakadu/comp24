(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Angstrom
open Ast

val parse_skip_fun : typexpr t
val parse_typexpr : typexpr t
val parse_typexpr_str : string -> (typexpr, string) result

(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Angstrom
open Ast

val parse_let_main_part : expression t -> (rec_flag * let_binding list) t
val parse_expr : expression t

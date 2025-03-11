(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Base

val eliminate_funs : expression -> pattern list * expression
val pattern_vars : pattern -> string list
val construct_from_pats : expression -> pattern list -> expression
val empty_set : (string, String.comparator_witness) Set.t
val set : string list -> (string, String.comparator_witness) Set.t
val expr_vars : expression -> (string, String.comparator_witness) Set.t
val closure_conversion : string list -> structure_item -> structure_item * string list
val convert_program : structure_item list -> structure_item list

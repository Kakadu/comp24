(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom

val is_keyword : string -> bool
val pattern : pattern t
val ident_as_expr : expr t
val expr : expr t
val program : structure t
val run : string -> (structure, string) Result.t
val expr_with_ops : expr t -> expr t
val show_res : input:string -> parser:'a t -> to_string:('a -> string) -> string

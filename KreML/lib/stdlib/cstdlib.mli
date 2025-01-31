(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

val print_int : ident
val is_stdlib_fun : ident -> bool
val stdlib_funs : ident list

type stdlib_fun = ident * int (* name, arity *)

val stdlib_funs_with_arity : stdlib_fun list

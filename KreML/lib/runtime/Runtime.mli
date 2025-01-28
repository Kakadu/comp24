(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(** This file contains runtime functions names *)

val alloc_closure : ident
val alloc_tuple : ident
val call_closure : ident
val list_cons : ident
val partial_match : ident
val partial_match_error : expr -> expr
val runtime_funs : ident list
val is_runtime_fun : ident -> bool


(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(** This file contains runtime functions names *)


val alloc_closure : ident
val alloc_tuple : ident
val call_closure : ident
val list_cons : ident
val list_head : ident
val list_tail : ident
val runtime_error : ident -> expr
val runtime_funs : ident list
val is_runtime_fun : ident -> bool
val stdlib_funs : ident list
val is_stdlib_fun : ident -> bool

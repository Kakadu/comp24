(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm

val block_terminates_with_unreachable : llbasicblock -> bool
val cast_firstclass_value : lltype -> llvalue -> lltype -> lltype -> llbuilder -> llvalue

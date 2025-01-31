(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Flambda

val dump : flstructure -> unit
val get_module : flstructure -> Llvm.llmodule

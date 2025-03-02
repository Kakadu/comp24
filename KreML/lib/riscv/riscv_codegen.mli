(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Flambda
open Riscv

val codegen_program
  :  flstructure * (string, Flambda.flambda, Base.String.comparator_witness) Base.Map.t
  -> instruction list

val dump : instruction list -> unit

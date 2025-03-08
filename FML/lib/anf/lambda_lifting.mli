(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Common
open Pe_ast

val run_ll
  :  bindings
  -> int
  -> pe_declaration list
  -> bindings * int * pe_declaration list

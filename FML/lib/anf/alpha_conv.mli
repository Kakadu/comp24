(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Pe_ast
open Common

val run_alpha_conv
  :  bindings
  -> int
  -> pe_declaration list
  -> bindings * int * pe_declaration list

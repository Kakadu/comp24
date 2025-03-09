(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Pe_ast
open Common

val run_pe : program -> bindings * int * pe_program

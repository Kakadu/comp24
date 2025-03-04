(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Llast

(** [run_ll_program] run process of lambda lifting on program *)
val run_ll_program : declaration list -> ll_decl list

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(** [run_closure_expression expr] applies closure conversion to a single expression [expr]. *)
val run_closure_expression : AbstractSyntaxTree.expression -> AbstractSyntaxTree.expression

(** [run_closure_program prog] applies closure conversion to a list of program declarations [prog]. *)
val run_closure_program : AbstractSyntaxTree.declaration list -> AbstractSyntaxTree.declaration list

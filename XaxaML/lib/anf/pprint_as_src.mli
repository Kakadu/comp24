(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

val pp_expr_as_src : Format.formatter -> Ast.expr -> unit
val pp_program_as_src : Format.formatter -> Ast.toplevel list -> unit

(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type bindings = Args_body of Ast.pattern list * Ast.expr

val collect_bindings_from_pat
  :  Ast.pattern
  -> (string, Base.String.comparator_witness) Base.Set.t

val collect_function_arguments : Ast.pattern list -> Ast.expr -> bindings
val prog_lift : Ast.let_declaration list -> Llast.lllet_declaration list * int
val lift_ast : Ast.let_declaration list -> Llast.lllet_declaration list

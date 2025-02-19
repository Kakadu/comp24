(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type context =
  { name_mapping : (string, string * int, Base.String.comparator_witness) Base.Map.t
  ; reserved_names : (string, Base.String.comparator_witness) Base.Set.t
  }

val alpha_convert_decl_list
  :  context
  -> Ast.let_declaration list
  -> Ast.let_declaration list
  -> ('a, Ast.let_declaration list) Common.StateMonad.t

val init_context : context
val test_alpha_for_decls : string -> unit

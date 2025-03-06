(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

val anf_decl
  :  (string, Base.String.comparator_witness) Base.Set.t
  -> Llast.lllet_declaration
  -> int
  -> int * (Anf_ast.anf_decl, string) result

val transform : Llast.lllet_declaration list -> (Anf_ast.anf_decl, string) result list

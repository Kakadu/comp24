(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type context = { reserved_names : (string, Base.String.comparator_witness) Base.Set.t }

val empty_context : context

val eliminate_match_in_declarations
  :  Llast.lllet_declaration list
  -> Llast.lllet_declaration list
  -> (context, Llast.lllet_declaration list) Common.StateMonad.t

(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type scheme =
  (Typing.type_variable_number, Base.String.comparator_witness) Base.Set.t * Typing.typ

val run_inference
  :  AstLib.Ast.decl list
  -> ((string, scheme, Base.String.comparator_witness) Base.Map.t, Typing.error) result

val print_env : ((string, 'a * Typing.typ, 'b) Base.Map.t, Typing.error) result -> unit

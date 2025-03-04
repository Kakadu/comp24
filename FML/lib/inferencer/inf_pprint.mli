(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Typedtree

val print_inferencer_error
  :  [< `InvalidRecLeftHand
     | `Not_impl
     | `Occurs_check of type_var * typ
     | `Several_bounds of string
     | `Unbound_variable of string
     | `Unification_failed of typ * typ
     ]
  -> unit

val print_expr_type : typ -> unit
val print_program_type : (string, scheme, 'a) Base.Map.t -> string list -> unit

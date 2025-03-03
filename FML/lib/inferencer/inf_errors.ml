(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Typedtree

type error =
  [ `Occurs_check of type_var * typ
  | `Unification_failed of typ * typ
  | `Unbound_variable of string
  | `Several_bounds of string
  | `Not_impl
  | `InvalidRecLeftHand
  ]

(** Copyright 2024, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open TypeTree

type error =
  | Occurs_check
  | Unbound_variable of string (* !!! *)
  | Unification_failed of typ * typ
  | InvalidRecursionLeftHand
  | Several_bounds of string (* !!! *)
(* [@@deriving show { with_path = false }] *)

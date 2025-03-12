(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open TypeTree

type error =
  | Occurs_check
  (* Trying to unify two types when one of the types contains the other type as a subtype or variable. *)
  | Unbound_variable of string (* An undeclared variable is used. *)
  | Unification_failed of typ * typ (* Castable types are not compatible. *)
  | InvalidRecursionLeftHand
  (* The left-hand side of a recursive let binding is not a simple variable.  e.g., let (x, y) = ... in ... *)
  | Several_bounds of string (* A variable in pattern is assigned more than once. *)

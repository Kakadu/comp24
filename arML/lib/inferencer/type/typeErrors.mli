(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  | Occurs_check
  (** Trying to unify two types when one of the types contains the other type as a subtype or variable. *)
  | Unbound_variable of string (** An undeclared variable is used. *)
  | Unification_failed of TypeTree.typ * TypeTree.typ
  (** Castable types are not compatible. *)
  | InvalidRecursionLeftHand
  (** The left-hand side of a recursive let binding is not a simple variable.  e.g., let (x, y) = ... in ... *)
  | Several_bounds of string (** A type variable is assigned more than once. *)

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Checks whether the passed type variable is contained in the passed type. *)
val occurs_check : int -> TypeTree.typ -> bool

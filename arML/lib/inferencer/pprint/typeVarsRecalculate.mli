(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Recalculates type variables in a type expression.
    This function renames type variables sequentially (starting from 0)
    to standardize type representations. *)
val recalculate_vars : TypeTree.typ -> TypeTree.typ

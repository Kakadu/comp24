(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** A schema is a pair of a set of type and closed in this type variables*)
type schema = Schema of TypeTree.TypeVarSet.t * TypeTree.typ

(** Returns the set of free type variables in a schema *)
val free_vars : schema -> TypeTree.TypeVarSet.t

(** Applies a substitution to a schema *)
val apply : Substitution.t -> schema -> schema

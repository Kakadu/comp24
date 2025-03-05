(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** A type environment that maps identifiers to their type schemas. *)
type t = (string, Schema.schema, Base.String.comparator_witness) Base.Map.t

(** An empty type environment. *)
val empty : t

(** Returns the set of free type variables in the environment. *)
val free_vars : t -> TypeTree.TypeVarSet.t

(** Applies a substitution to all type schemas in the environment. *)
val apply : t -> Substitution.t -> t

(** Extends the environment by adding or updating a schema for a given identifier. *)
val extend : t -> string -> Schema.schema -> t

(** Looks up the schema for a given identifier in the environment. *)
val find : t -> string -> Schema.schema option

(** Looks up an identifier in the environment and returns its type.
    If the identifier is not found, an error is returned. *)
val lookup_env
  :  t
  -> string
  -> (Substitution.t * TypeTree.typ, TypeErrors.error) Common.StateResultMonad.t

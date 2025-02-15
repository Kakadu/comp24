(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** A substitution mapping type variable identifiers to types. *)
type t = (int, TypeTree.typ, Base.Int.comparator_witness) Base.Map.t

(** The empty substitution. *)
val empty : t

(** Returns a substitution mapping type variable [k] to type [v].
    Fails if [v] contains [k]. *)
val singleton
  :  int
  -> TypeTree.typ
  -> ( (int, TypeTree.typ, Base.Int.comparator_witness) Base.Map.t
       , TypeErrors.error )
       Common.StateResultMonad.t

(** Returns the substitution binding for type variable [k], if any. *)
val find : t -> int -> TypeTree.typ option

(** Removes the binding for type variable [k] from the substitution. *)
val remove : t -> int -> t

(** Applies a substitution to a type. *)
val apply : t -> TypeTree.typ -> TypeTree.typ

(** Unifies two types, returning a substitution on success. *)
val unify
  :  TypeTree.typ
  -> TypeTree.typ
  -> (t, TypeErrors.error) Common.StateResultMonad.t

(** Extends a substitution with a new binding from [k] to [v]. *)
val extend : int -> TypeTree.typ -> t -> (t, TypeErrors.error) Common.StateResultMonad.t

(** Composes two substitutions. *)
val compose : t -> t -> (t, TypeErrors.error) Common.StateResultMonad.t

(** Composes a list of substitutions. *)
val compose_all : t list -> (t, TypeErrors.error) Common.StateResultMonad.t

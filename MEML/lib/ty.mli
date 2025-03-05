(** Copyright 2024-2025, Perevalov Efim, Dyachkov Vitaliy *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

type binder = int

module VarSet : sig
  type elt = binder
  type t

  val pp : formatter -> t -> unit
  val empty : t
  val add : elt -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val mem : elt -> t -> bool
  val diff : t -> t -> t
  val union : t -> t -> t
end

type ty =
  | TBool
  | TInt
  | TVar of binder * Ast.type_of_var
  | TArrow of ty * ty
  | TList of ty
  | TTuple of ty list

type binder_set = VarSet.t
type scheme = S of binder_set * ty

val pp_scheme : formatter -> scheme -> unit
val pp_typ : formatter -> ty -> unit
val print_typ : ty -> unit

type error =
  [ `Occurs_check
  | `Empty_pattern
  | `No_variable of string
  | `Unification_failed of ty * ty
  ]

val print_typ_err : error -> unit

(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module VarSet : sig
  type elt = int
  type t = Set.Make(Int).t

  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val exists : (elt -> bool) -> t -> bool
  val pp : Format.formatter -> t -> unit
end

type binder = int

val equal_binder : binder -> binder -> bool
val pp_binder : Format.formatter -> binder -> unit
val show_binder : binder -> string

type binder_set = VarSet.t

val pp_binder_set : Format.formatter -> binder_set -> unit
val show_binder_set : binder_set -> string

type ty =
  | TPrim of string
  | TVar of binder
  | TTuple of ty list
  | TArrow of ty * ty
  | TList of ty

val equal_ty : ty -> ty -> bool
val pp_ty : Format.formatter -> ty -> unit
val show_ty : ty -> string

type scheme = S of binder_set * ty

val pp_scheme : Format.formatter -> scheme -> unit
val show_scheme : scheme -> string

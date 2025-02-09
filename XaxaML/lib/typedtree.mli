(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type typ =
  | T_var of int
  | T_prim of string
  | T_arr of typ * typ
  | T_tuple of typ * typ list
  | T_list of typ

module TypeVarSet : sig
  type elt = int
  type t = Set.Make(Int).t

  val empty : t
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (elt -> unit) -> t -> unit
  val is_empty : t -> bool
end

type scheme = Scheme of TypeVarSet.t * typ

val type_var : int -> typ
val int_typ : typ
val bool_typ : typ
val unit_typ : typ
val arrow : typ -> typ -> typ
val ( @-> ) : typ -> typ -> typ

(* pretty print types with letters *)
val pp_typ : Format.formatter -> typ -> unit

(* pretty print types without mapping to letters *)
val pp_typ_debug : Format.formatter -> typ -> unit

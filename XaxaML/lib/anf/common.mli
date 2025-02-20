(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module StrSet : sig
  type t

  val empty : t
  val singleton : string -> t
  val union : t -> t -> t
  val union_list : t list -> t
  val add : t -> string -> t
  val find : t -> string -> bool
  val to_list : t -> string list
  val of_list : string list -> t
  val fold : t -> init:'a -> f:('a -> string -> 'a) -> 'a
  val diff : t -> t -> t
end

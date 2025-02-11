(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base

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
end = struct
  type t = (string, String.comparator_witness) Set.t

  let empty = Set.empty (module String)
  let singleton str = Set.singleton (module String) str
  let union = Set.union
  let union_list lst = Set.union_list (module String) lst

  let find s str =
    match Set.binary_search s ~compare:String.compare `First_equal_to str with
    | Some _ -> true
    | None -> false
  ;;

  let add = Set.add
  let to_list = Set.to_list
  let of_list = Set.of_list (module String)
  let fold = Set.fold
  let diff = Set.diff
end

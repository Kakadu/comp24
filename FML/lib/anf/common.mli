(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

module StrMap : sig
  type 'a t

  val empty : 'a t
  val singleton : string -> 'a -> 'a t
  val update : 'a t -> string -> f:('a option -> 'a) -> 'a t
  val find : 'a t -> string -> 'a option
  val merge_two : 'a t -> 'a t -> 'a t
end

module StrSet : sig
  type t

  val add : t -> string -> t
  val empty : t
  val singleton : string -> t
  val union : t -> t -> t
  val to_list : t -> string list
  val of_list : string list -> t
  val diff : t -> t -> t
  val union_list : t list -> t
  val find : t -> string -> bool
end

val builtins : string list

module StateMonad : sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  module RList : sig
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
    val fold_right : 'a list -> init:'b t -> f:('a -> 'b -> 'b t) -> 'b t
  end

  module RMap : sig
    val fold_left
      :  ('a, 'b, 'c) Base.Map.t
      -> init:'d t
      -> f:('a -> 'b -> 'd -> 'd t)
      -> 'd t
  end

  val fresh : int t
  val run : 'a t -> 'a
end

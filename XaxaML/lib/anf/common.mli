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

module MonadCounter : sig
  type 'a t

  val return : 'a -> 'a t
  val fresh : int t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val run : 'a t -> int -> int * 'a

  module RList : sig
    val map : 'a list -> f:('a -> 'b t) -> 'b list t
    val fold_left : 'a list -> init:'b t -> f:('b -> 'a -> 'b t) -> 'b t
  end
end

module MonadCounterError : sig
  type ('a, 'e) t

  val return : 'a -> ('a, 'e) t
  val fresh : (int, 'e) t
  val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
  val run : int -> ('a, 'e) t -> ('a, 'e) result

  module RList : sig
    val map : 'a list -> f:('a -> ('b, 'e) t) -> ('b list, 'e) t
    val fold_left : 'a list -> init:('b, 'e) t -> f:('b -> 'a -> ('b, 'e) t) -> ('b, 'e) t

    val fold_right
      :  'a list
      -> init:('b, 'e) t
      -> f:('a -> 'b -> ('b, 'e) t)
      -> ('b, 'e) t
  end

  module RMap : sig
    val fold
      :  ('a, 'b, 'c) Base.Map.t
      -> init:('d, 'e) t
      -> f:('d -> 'a -> 'b -> ('d, 'e) t)
      -> ('d, 'e) t
  end
end

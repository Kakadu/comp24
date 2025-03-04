(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base

type ('a, 'e) t = int -> int * ('a, 'e) Result.t

val ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
val ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
val return : 'a -> ('a, 'e) t
val fail : 'e -> ('a, 'e) t
val bind : ('a, 'e) t -> f:('a -> ('b, 'e) t) -> ('b, 'e) t
val fresh : (int, 'e) t

module Syntax : sig
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

module RMap : sig
  val fold_left
    :  ('a, 'b, 'c) Base.Map.t
    -> init:('d, 'e) t
    -> f:('a -> 'b -> 'd -> ('d, 'e) t)
    -> ('d, 'e) t
end

module RList : sig
  val fold_left : 'a list -> init:('b, 'e) t -> f:('b -> 'a -> ('b, 'e) t) -> ('b, 'e) t
end

val run : ('a, 'e) t -> ('a, 'e) Result.t

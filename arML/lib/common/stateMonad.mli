(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type 'a t = int -> 'a * int

val return : 'a -> 'a t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val map : 'a t -> ('a -> 'b) -> 'b t

val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

val (>>|) : 'a t -> ('a -> 'b) -> 'b t

val get : int t

val put : int -> unit t

val fresh : int t

module Syntax : sig
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

val run : 'a t -> int -> 'a * int

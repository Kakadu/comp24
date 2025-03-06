(** Copyright 2024-2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module type CounterMod = sig
  type t

  val add : t -> int -> t
end

module type S = sig
  type counter
  type error
  type 'a t = counter -> counter * ('a, error) Result.t

  val return : 'a -> 'a t
  val fail : error -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val map : ('a -> 'b t) -> 'a list -> 'b list t
  val fold_left : ('a -> 'b -> 'a t) -> 'a t -> 'b list -> 'a t
  val fresh : counter t
  val run : 'a t -> counter -> ('a, error) Result.t
end

module Make
    (A : CounterMod)
    (E : sig
       type t
     end) : S with type counter = A.t with type error = E.t

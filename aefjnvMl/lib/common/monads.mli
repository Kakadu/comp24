(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module Base_SE_Monad : functor (StateT : Base.T) (ErrorT : Base.T) -> sig
  type 'a t = StateT.t -> StateT.t * ('a, ErrorT.t) result

  val run : ('a -> 'b) -> 'a -> 'b
  val fail : 'a -> 'b -> 'b * ('c, 'a) result
  val return : 'a -> 'b -> 'b * ('a, 'c) result
  val read : 'a -> 'a * ('a, 'b) result
  val save : 'a -> 'b -> 'a * (unit, 'c) result

  val ( >>= )
    :  ('a -> 'b * ('c, 'd) result)
    -> ('c -> 'b -> 'b * ('e, 'd) result)
    -> 'a
    -> 'b * ('e, 'd) result

  val ( let* )
    :  ('a -> 'b * ('c, 'd) result)
    -> ('c -> 'b -> 'b * ('e, 'd) result)
    -> 'a
    -> 'b * ('e, 'd) result

  val ( >>| ) : ('a -> 'b * ('c, 'd) result) -> ('c -> 'e) -> 'a -> 'b * ('e, 'd) result
  val ( let+ ) : ('a -> 'b * ('c, 'd) result) -> ('c -> 'e) -> 'a -> 'b * ('e, 'd) result

  val fold_left_t
    :  'a list
    -> init:('b -> 'c * ('d, 'e) result)
    -> f:('d -> 'a -> 'c -> 'c * ('d, 'e) result)
    -> 'b
    -> 'c * ('d, 'e) result

  val revt : ('a -> 'b * ('c list, 'd) result) -> 'a -> 'b * ('c list, 'd) result

  val mapt
    :  'a list
    -> ('a -> 'b -> 'b * ('c, 'd) result)
    -> 'b
    -> 'b * ('c list, 'd) result

  val ignore_t : ('a -> 'b * ('c, 'd) result) -> 'a -> 'b * (unit, 'd) result
end

module CounterMonad : functor (ErrorT : Base.T) -> sig
  include module type of Base_SE_Monad (Int) (ErrorT)

  val fresh : int t
end

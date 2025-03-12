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
    (Counter : CounterMod)
    (E : sig
       type t
     end) =
struct
  type error = E.t
  type counter = Counter.t
  type 'a t = counter -> counter * ('a, error) Result.t

  let fail e st = st, Error e
  let return x last = last, Ok x

  let ( >>= ) (m : 'a t) (f : 'a -> 'b t) st =
    let last, r = m st in
    match r with
    | Error x -> last, Error x
    | Ok a -> f a last
  ;;

  let ( >>| ) x f = x >>= fun a -> return @@ f a
  let ( let* ) x f = x >>= f
  let ( let+ ) x f = x >>| f

  let ( and+ ) (o1 : 'a t) (o2 : 'b t) st =
    let res1 = o1 st in
    let res2 = o2 (fst res1) in
    ( fst res2
    , match snd res1, snd res2 with
      | Ok a, Ok b -> Ok (a, b)
      | Ok _, (Error _ as a) -> a
      | (Error _ as a), _ -> a )
  ;;

  let rec map (f : 'a -> 'b t) = function
    | h :: tl ->
      let+ h = f h
      and+ tl = map f tl in
      h :: tl
    | _ -> return []
  ;;

  let rec fold_left (f : 'a -> 'b -> 'a t) (acc : 'a t) : 'b list -> 'a t = function
    | h :: tl ->
      let* acc = acc in
      fold_left f (f acc h) tl
    | _ -> acc
  ;;

  let fresh (last : counter) = Counter.add last 1, Ok last
  let run p start_value : ('a, error) Result.t = snd (p start_value)
end

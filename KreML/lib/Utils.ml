(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let internalfail = failwith
let unreachable () = internalfail "Reached unreachable by assumption code"

module type Monad = sig
  type 'a t 
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (let*) : 'a t -> ('a -> 'b t) -> 'b t
end


module type STATE = sig
  include Monad
  type state
  val get : state t
  val put : state -> state t
  val run : 'a t -> state -> state *  'a
end

module State (S : sig type t end) : STATE with type state = S.t = struct
  type state = S.t
  type 'a t = state -> state * 'a
  let return v st = st, v
  let (>>=) m f st =
    let s1, v = m st in
    f v s1
  let (let*) = (>>=)
  let get s = s, s
  let put s v = s, v
  let run m st = m st
end


module Counter = State (struct type t = int end)


let fresh_name base : string Counter.t =
  let open Counter in
  get            >>= fun curr ->
  put (curr + 1) >>= fun u ->
  Format.sprintf "%s_%i" base u |> return


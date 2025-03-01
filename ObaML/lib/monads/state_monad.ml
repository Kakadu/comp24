(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module StateMonad = struct
  type 'a state = int -> int * 'a

  let ( >>= ) : 'a 'b. 'a state -> ('a -> 'b state) -> 'b state =
    fun m f st ->
    let last, r = m st in
    f r last
  ;;

  let bind x ~f = x >>= f
  let return x last = last, x
  let ( let* ) x f = bind x ~f

  let ( >>| ) : 'a 'b. 'a state -> ('a -> 'b) -> 'b state =
    fun x f st ->
    match x st with
    | st, x -> st, f x
  ;;

  let fresh last = last + 1, last
  let run m = snd (m 0)
end

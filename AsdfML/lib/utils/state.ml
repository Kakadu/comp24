(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base

module StateM = struct
  type ('state, 'a) t = 'state -> 'a * 'state

  let return (x : 'a) : ('state, 'a) t = fun s -> x, s

  let bind (m : ('state, 'a) t) (f : 'a -> ('state, 'b) t) : ('state, 'b) t =
    fun s ->
    let a, new_state = m s in
    f a new_state
  ;;

  let get : ('state, 'state) t = fun s -> s, s
  let put (new_state : 'state) : ('state, unit) t = fun _ -> (), new_state
  let run (m : ('state, 'a) t) (init : 'state) : 'a * 'state = m init
  let modify (f : 'state -> 'state) : ('state, unit) t = bind get (fun s -> put (f s))

  module Syntax = struct
    let ( >>= ) = bind
    let ( let* ) = bind

    let ( >>| ) : ('state, 'a) t -> ('a -> 'b) -> ('state, 'b) t =
      fun m f state ->
      let value, st = m state in
      f value, st
    ;;
  end
end

module IntStateM = struct
  include StateM

  type 'a t = (int, 'a) StateM.t

  let fresh last = last, last + 1
  let fresh_prefix str last = str ^ "_" ^ string_of_int last, last + 1
  let run m = fst (m 0)
end

open StateM
open StateM.Syntax

let mfold xs ~init ~f =
  List.fold xs ~init:(return init) ~f:(fun acc x -> acc >>= fun acc -> f acc x)
;;

let mfoldi xs ~init ~f =
  List.foldi xs ~init:(return init) ~f:(fun idx acc x -> acc >>= fun acc -> f idx acc x)
;;

let mfold_right xs ~init ~f =
  List.fold_right xs ~init:(return init) ~f:(fun x acc -> acc >>= fun acc -> f x acc)
;;

let mfoldi_right xs ~init ~f =
  let len = List.length xs - 1 in
  snd
    (List.fold_right
       xs
       ~init:(len, return init)
       ~f:(fun v (i, acc) -> i - 1, acc >>= fun acc -> f i acc v))
;;

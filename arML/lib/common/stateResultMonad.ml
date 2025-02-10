(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base

type ('a, 'e) t = int -> int * ('a, 'e) Result.t (* State and Result monad composition *)

let ( >>= ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t =
  fun m f s ->
  match m s with
  | s, Result.Error e -> s, Error e
  | s, Result.Ok v -> f v s
;;

let ( >>| ) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t =
  fun m f s ->
  match m s with
  | s, Result.Error e -> s, Error e
  | s, Result.Ok v -> s, Base.Result.return @@ f v
;;

let return v last = last, Base.Result.return v
let fail e state = state, Base.Result.fail e
let bind x ~f = x >>= f
let fresh last = last + 1, Result.Ok last (* Get new state *)

module Syntax = struct
  let ( let* ) x f = bind x ~f (* Syntax sugar for bind *)
end

module RMap = struct
  (* Classic map folding. *)
  let fold_left mp ~init ~f =
    let open Syntax in
    Base.Map.fold mp ~init ~f:(fun ~key ~data acc ->
        let* acc = acc in
        f key data acc)
  ;;
end

module RList = struct
  (* Classic list folding. *)
  let fold_left lt ~init ~f =
    let open Syntax in
    Base.List.fold_left lt ~init ~f:(fun acc item ->
        let* acc = acc in
        f acc item)
  ;;
end

(* Run and get the internal value. *)
let run m = snd (m 0)

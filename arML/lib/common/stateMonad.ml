(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type 'a t = int -> 'a * int

let return x s = x, s

let bind m f s =
  let a, s' = m s in
  f a s'
;;

let map m f s =
  let a, s' = m s in
  f a, s'
;;

let ( >>= ) m f = bind m f
let ( >>| ) m f = map m f
let get s = s, s
let put new_s _ = (), new_s
let fresh s = s, s + 1

module Syntax = struct
  let ( let* ) x f = bind x f
end

let run m s = m s

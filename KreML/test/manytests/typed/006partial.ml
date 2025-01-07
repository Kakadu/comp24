(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let foo b = if b then fun foo -> foo + 2 else fun foo -> foo * 10
let foo x = foo true (foo false (foo true (foo false x)))

let main =
  let () = print_int (foo 11) in
  0
;;

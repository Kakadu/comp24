(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let foo a =
  let () = print_int a in
  fun b ->
    let () = print_int b in
    fun c -> print_int c
;;

let main =
  let () = foo 4 8 9 in
  0
;;

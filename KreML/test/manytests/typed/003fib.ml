(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let rec fib_acc a b n =
  if n = 1
  then b
  else (
    let n1 = n - 1 in
    let ab = a + b in
    fib_acc b ab n1)
;;

let rec fib n = if n < 2 then n else fib (n - 1) + fib (n - 2)

let main =
  let () = print_int (fib_acc 0 1 4) in
  let () = print_int (fib 4) in
  0
;;

(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let addi f g x : int = f x (g x : bool)

let main =
  let () =
    print_int
      (addi (fun x b -> if b then x + 1 else x * 2) (fun _start -> _start / 2 = 0) 4)
  in
  0
;;

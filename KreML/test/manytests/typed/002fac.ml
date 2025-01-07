(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let rec fac_cps n k = if n = 1 then k 1 else fac_cps (n - 1) (fun p -> k (p * n))

let main =
  let () = print_int (fac_cps 4 (fun print_int -> print_int)) in
  0
;;
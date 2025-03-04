(* * Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  lambda_lifting_program s
;;

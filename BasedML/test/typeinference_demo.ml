(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  let res = Parser.parse_program s in
  match res with
  | Result.Ok prog ->
    let res = Typeinference.infer_prog prog in
    (match res with
     | Result.Ok map -> Format.printf "%s@\n" (Typeinference.show_res_map map)
     | Result.Error s -> Format.printf "Infer error: %s" s)
  | Result.Error e -> Format.printf "Parser error: %s" e
;;

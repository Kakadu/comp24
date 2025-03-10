(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.parse_program s with
  | Ok ast ->
    let transformed = Middleend.Full.middleend_transform_prog ast in
    (match transformed with
     | Ok new_ast -> Format.printf "%s\n" (Middleend.Anf_to_str.program_to_string new_ast)
     | Error message -> Format.printf "Error: %s\n" message)
  | Error message -> Format.printf "Error: %s\n" message
;;

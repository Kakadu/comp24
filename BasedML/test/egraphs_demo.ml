(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.parse_program s with
  | Ok ast ->
    let simplified = Middleend.Egraphs.simplify ast in
    (match simplified with
     | Ok decls ->
       Restore_src.RestoreSrc.restore_declarations decls |> Format.print_string
     | Error message -> Format.printf "Error: %s\n" message)
  | Error message -> Format.printf "Error: %s\n" message
;;

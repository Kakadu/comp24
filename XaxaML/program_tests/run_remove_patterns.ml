(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open XaxaML

let () =
  let str = Stdio.In_channel.input_all Stdlib.stdin in
  let parsed = Parser.run_parser_program str in
  match parsed with
  | Result.Error err -> Format.printf "Parsing error%s\n" err
  | Result.Ok parsed ->
    (match Inferencer.run_infer_program parsed with
     | Error err -> Format.printf "Type inference error: %a\n" Inferencer.pp_error err
     | Ok _ ->
       let ast = Remove_patterns.run_remove_patterns_program parsed in
       Format.printf "%a" Remove_patterns.pp_rp_program ast)
;;

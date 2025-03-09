(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let infer_and_print s =
  let open TypeInference in
  match Parser.parse_program s with
  | Ok actual ->
    let env = Inferencer.run_inference actual in
    Inferencer.print_env env
  | Error err -> Format.printf "%s\n" err
;;

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  infer_and_print s
;;

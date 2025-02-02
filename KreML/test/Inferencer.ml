(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Kreml_lib.Inferencer

let () =
  let input = In_channel.input_all Stdlib.stdin in
  let open Stdlib.Format in
  match Kreml_lib.Parser.run input with
  | Ok s ->
    (match Kreml_lib.Inferencer.run s with
     | Ok env -> fprintf std_formatter "%a" TypeEnv.pp env
     | Error e -> fprintf std_formatter "%a" pp_error e)
  | Error msg -> fprintf std_formatter "%s" msg
;;

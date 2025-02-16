(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Cursedml_lib.Inferencer

let () =
  let input = In_channel.input_all Stdlib.stdin in
  let open Stdlib.Format in
  match Cursedml_lib.Parser.run input with
  | Ok s ->
    (match Cursedml_lib.Inferencer.run s with
     | Ok env -> fprintf std_formatter "%a" TypeEnv.pp env
     | Error e -> fprintf std_formatter "%a" pp_error e)
  | Error msg -> fprintf std_formatter "%s" msg
;;

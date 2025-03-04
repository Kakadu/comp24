(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Fml_lib.Parser
open Fml_lib.Inferencer
open Fml_lib.Inf_pprint

let () =
  let input = Stdio.In_channel.input_all Stdlib.stdin in
  match parse input with
  | Ok ast ->
    (match run_program_inferencer ast with
     | Ok (env, names_list) -> print_program_type env names_list
     | Error e -> print_inferencer_error e)
  | Error message -> Format.printf "Error: %s\n" message
;;

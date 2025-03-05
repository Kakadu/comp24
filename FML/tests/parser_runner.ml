(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Fml_lib.Parser
open Fml_lib.Ast

let () =
  let input = Stdio.In_channel.input_all Stdlib.stdin in
  match parse input with
  | Ok ast -> Format.printf "%a\n" pp_program ast
  | Error message -> Format.printf "Error: %s\n" message
;;

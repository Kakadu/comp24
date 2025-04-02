(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Fml_lib.Parser
open Fml_lib.Inferencer
open Fml_lib.A_conv
open Fml_lib.Match_elimination
open Fml_lib.Pe_ast

let () =
  let input = Stdio.In_channel.input_all Stdlib.stdin in
  let parse_and_infer input =
    match parse input with
    | Ok parsed ->
      (match run_program_inferencer parsed with
       | Ok _ -> Ok parsed
       | Error _ -> Error (Format.asprintf "Infer error:"))
    | Error e -> Error (Format.sprintf "Parsing error: %s" e)
  in
  match parse_and_infer input with
  | Ok ast ->
    let ast = ac_program ast in
    let ast_pe = match_elimination ast in
    Format.printf "%a\n" pp_pe_program ast_pe
  | Error message -> Format.printf "%s" message
;;

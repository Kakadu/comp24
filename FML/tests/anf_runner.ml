(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Fml_lib.Parser
open Fml_lib.Inferencer
open Fml_lib.Inf_pprint
open Fml_lib.Pattern_elim
open Fml_lib.Alpha_conv
open Fml_lib.Lambda_lifting
open Fml_lib.Closure_conv
open Fml_lib.Anf
open Fml_lib.Anf_ast

let () =
  let input = Stdio.In_channel.input_all Stdlib.stdin in
  let parse_and_infer input =
    match parse input with
    | Ok parsed ->
      (match run_program_inferencer parsed with
       | Ok types -> Ok (parsed, types)
       | Error _ -> Error (Format.asprintf "Infer error."))
    | Error e -> Error (Format.sprintf "Parsing error: %s" e)
  in
  match parse_and_infer input with
  | Ok (ast, _) ->
    let bind, cnt, ast = run_pe ast in
    let bind, cnt, ast = run_alpha_conv bind cnt ast in
    let ast = run_cc ast in
    let bind, cnt, ast = run_ll bind cnt ast in
    let _, _, ast = run_anf bind cnt ast in
    let result = Format.asprintf "%a" pp_anf_program ast in
    let () = Format.printf "%a" pp_anf_program ast in
    let () = Format.printf "\nТипы после приведения в ANF:\n" in
    (match parse_and_infer result with
     | Ok (_, (env, names_list)) -> print_program_type env names_list
     | Error e -> Format.printf "%s" e)
  | Error message -> Format.printf "%s" message
;;

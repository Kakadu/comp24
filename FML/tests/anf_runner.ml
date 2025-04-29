(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Fml_lib.Parser
open Fml_lib.Inferencer
open Fml_lib.Inf_pprint
open Fml_lib.A_conv
open Fml_lib.Match_elimination
open Fml_lib.C_conv
open Fml_lib.Anf
open Fml_lib.Lambda_lift
open Fml_lib.Anf_ast

let () =
  let input = Stdio.In_channel.input_all Stdlib.stdin in
  let parse_and_infer input =
    match parse input with
    | Ok parsed ->
      (match run_program_inferencer parsed with
       | Ok types -> Ok (parsed, types)
       | Error _ -> Error (Format.asprintf "Infer error:"))
    | Error e -> Error (Format.sprintf "Parsing error: %s" e)
  in
  match parse_and_infer input with
  | Ok (ast, (env, names_list)) ->
    let ast = ac_program ast in
    let ast_me = match_elimination ast in
    let ast_cc = cc_program ast_me in
    let ast_ll = lambda_lift ast_cc in
    let ast_anf = anf ast_ll in
    let result = Format.asprintf "%a" pp_anf_program ast_anf in
    let () = Format.printf "%a\n" pp_anf_program ast_anf in
    let () = Format.printf "\nТипы до приведения в ANF:\n" in
    let () = print_program_type env names_list in
    let () = Format.printf "\nТипы после приведения в ANF:\n" in
    (match parse_and_infer result with
     | Ok (_, (env, names_list)) -> print_program_type env names_list
     | Error e -> Format.printf "%s" e)
  | Error message -> Format.printf "%s" message
;;

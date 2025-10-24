(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Fml_lib

let () =
  let input = Stdio.In_channel.input_all Stdlib.stdin in
  let parse_and_infer input =
    match Parser.parse input with
    | Ok parsed ->
      (match Inferencer.run_program_inferencer parsed with
       | Ok types -> Ok (parsed, types)
       | Error _ -> Error (Format.asprintf "Infer error:"))
    | Error e -> Error (Format.sprintf "Parsing error: %s" e)
  in
  match parse_and_infer input with
  | Ok (ast, _) ->
    let ast = A_conv.ac_program ast in
    let ast_me = Match_elimination.match_elimination ast in
    let ast_cc = C_conv.cc_program ast_me in
    let ast_ll = Lambda_lift.lambda_lift ast_cc in
    let ast_anf = Anf.anf ast_ll in
    Codegen.compile_program ast_anf
  | Error message -> Format.printf "%s" message
;;

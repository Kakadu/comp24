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
       | Error _ -> Error (Format.asprintf "Infer error."))
    | Error e -> Error (Format.sprintf "Parsing error: %s" e)
  in
  match parse_and_infer input with
  | Ok (ast, _) ->
    let bind, cnt, ast = Pattern_elim.run_pe ast in
    let bind, cnt, ast = Alpha_conv.run_alpha_conv bind cnt ast in
    let ast = Closure_conv.run_cc ast in
    let bind, cnt, ast = Lambda_lifting.run_ll bind cnt ast in
    let _, _, ast = Anf.run_anf bind cnt ast in
    Codegen.compile_program ast
  | Error message -> Format.printf "%s" message
;;

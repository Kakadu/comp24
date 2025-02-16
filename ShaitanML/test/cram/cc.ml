(** Copyright 2024-2025, Nikita Lukonenko and Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Shaitanml_lib

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  let parse_and_infer s =
    match Parser.parse s with
    | Ok parsed as ast ->
      (match Infer.run_infer parsed with
       | Ok _ -> ast
       | Error e -> Error (Format.asprintf "Infer error: %a\n" Infer.pp_error e))
    | Error e -> Error (Format.sprintf "Parsing error: %s\n" e)
  in
  match parse_and_infer s with
  | Ok ast ->
    let converted = Closure.convert_ast ast in
    Ast.print_structure converted |> Format.print_string
  | Error message -> Format.printf "%s" message
;;

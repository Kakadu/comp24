(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Middleend

let test str =
  match Parser.Main_parser.parse str with
  | Result.Error e -> Format.eprintf "Parser error: %s" e
  | Result.Ok ast ->
    (match Typing.Inference.infer_program ast with
     | Result.Error s -> Format.eprintf "Inference error: %s" s
     | Result.Ok _ ->
       let ( >>= ) = Result.bind in
       let res =
         Pattern_remover.remove_patterns ast
         >>= Alpha_conversion.alpha_conversion
         |> Result.map_error Middleend_utils.exp_to_string
       in
       let res =
         match res with
         | Result.Ok s ->
           s |> Converter.rast_to_ast |> Quickcheck.Main_unparser.unparse_structure
         | Result.Error s -> s
       in
       print_string res)
;;

let () = test @@ Stdio.In_channel.input_all Stdlib.stdin

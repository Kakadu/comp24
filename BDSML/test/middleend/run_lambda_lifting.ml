(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

let test str =
  match Parser.Main_parser.parse str with
  | Result.Error e -> Format.eprintf "Parser error: %s" e
  | Result.Ok ast ->
    (match Typing.Inference.infer_program ast with
     | Result.Error s -> Format.eprintf "Inference error: %s" s
     | Result.Ok _ ->
       print_string
         (Middleend.Pattern_remover.remove_patterns ast
          |> Middleend.Alpha_conversion.alpha_conversion
          |> Middleend.Closure_conversion.closure_convert
          |> Middleend.Lambda_lifting.ll
          |> Middleend.Converter.rast_to_ast
          |> Quickcheck.Main_unparser.unparse_structure))
;;

let () = test @@ Stdio.In_channel.input_all Stdlib.stdin

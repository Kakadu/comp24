(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Middleend

let test str =
  match Parser.Main_parser.parse str with
  | Result.Error e -> Format.eprintf "Parser error: %s" e
  | Result.Ok ast ->
    let print_inference = function
      | Result.Error s ->
        Format.printf "Inference error: %s\n" s;
        Format.print_newline ()
      | Result.Ok s ->
        Typing.Types.print_types s;
        Format.print_newline ()
    in
    Format.print_string "Types before middleend:\n";
    let ( >>= ) = Result.bind in
    let inf_res = Typing.Inference.infer_program ast in
    print_inference inf_res;
    let res =
      inf_res
      >>= fun _ ->
      Pattern_remover.remove_patterns ast
      >>= Alpha_conversion.alpha_conversion
      |> Result.map Closure_conversion.closure_convert
      >>= Alpha_conversion.alpha_conversion
      >>= Lambda_lifting.ll
      >>= Anf.rast_to_anf
      |> Result.map_error Middleend_utils.exp_to_string
    in
    (match res with
     | Result.Ok s ->
       let ast = Converter.anf_to_ast s in
       let inf_res = Typing.Inference.infer_program ast in
       Format.print_string "Types after anf:\n";
       print_inference inf_res;
       Format.print_string @@ Quickcheck.Main_unparser.unparse_structure ast
     | Result.Error s -> Format.print_string s)
;;

let () = test @@ Stdio.In_channel.input_all Stdlib.stdin

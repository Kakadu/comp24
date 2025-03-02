(** Copyright 2024-2025, Nikita Lukonenko and Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Shaitanml_lib

(* let () =
  let str = Stdio.In_channel.input_all Stdlib.stdin in
  let parsed = Parser.run_parser_program str in
  match parsed with
  | Result.Error err -> Format.printf "Parsing error%s\n" err
  | Result.Ok parsed ->
    (match Inferencer.run_infer_program parsed with
     | Error err -> Format.printf "Type inference error: %a\n" Inferencer.pp_error err
     | Ok _ ->
       let nh, names_count, ast = Remove_patterns.run parsed in
       let _, _, ast = Alpha_conversion.run nh names_count ast in
       Format.printf "%a" Remove_patterns.PP.pp_rp_program ast)
;; *)

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
    let nh, names_count, ast = Pat_elim.run_pat_elim ast in
    let _, _, ast = Alpha.run_ac nh names_count ast in
    Format.printf "%a" Pat_elim_ast.pp_pe_structure ast
  | Error message -> Format.printf "%s" message
;;

(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML

let infer_and_print fmt structure =
  match Inferencer.run_structure_infer_with_custom_std structure Std.extended_std_lst with
  | Ok env ->
    Format.fprintf fmt "%a" Inferencer.TypeEnv.pretty_pp_env (Std.extended_std_lst, env)
  | Error err -> Format.fprintf fmt "Infer: %a" Typedtree.pp_error err
;;

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.structure_from_string s with
  | Ok structure ->
    let simple_structure = To_simple_ast.convert structure in
    let simple_structure = Alpha_conversion.run_alpha_conversion simple_structure Inner in
    let simple_structure = Closure_conversion.run_closure_conversion simple_structure in
    let simple_structure = Lambda_lifting.run_lambda_lifting simple_structure in
    let new_structure = To_ast.convert simple_structure in
    Format.printf
      "Types:\n%a\nConverted structure:\n%a\nTypes after conversions:\n%a"
      infer_and_print
      structure
      Simple_ast_pretty_printer.print_structure
      simple_structure
      infer_and_print
      new_structure
  | Error err -> Format.printf "Parser: %s\n" err
;;

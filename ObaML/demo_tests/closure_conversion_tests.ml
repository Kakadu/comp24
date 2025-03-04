(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.structure_from_string s with
  | Ok structure ->
    (match Inferencer.run_structure_infer structure with
     | Ok structure_env ->
       Format.printf
         "Types:\n%a\n"
         Inferencer.TypeEnv.pretty_pp_env
         (Std.std_lst, structure_env);
       let structure, _ =
         Alpha_conversion.run_alpha_conversion structure
       in
       let simple_structure = To_simple_ast.convert structure in
       let simple_structure =
         Closure_conversion.run_closure_conversion simple_structure
       in
       let new_structure = To_ast.convert simple_structure in
       Format.printf
         "Converted structure:\n%a\n"
         Simple_ast_pretty_printer.print_structure
         simple_structure;
       (match
          Inferencer.run_structure_infer_with_custom_std
            new_structure
            Std.extended_std_lst
        with
        | Ok new_structure_env ->
          Format.printf
            "Types after conversions:\n%a"
            Inferencer.TypeEnv.pretty_pp_env
            (Std.extended_std_lst, new_structure_env)
        | Error e -> Format.printf "Infer: %a" Typedtree.pp_error e)
     | Error e -> Format.printf "Infer: %a" Typedtree.pp_error e)
  | Error err -> Format.printf "Parser: %s\n" err
;;

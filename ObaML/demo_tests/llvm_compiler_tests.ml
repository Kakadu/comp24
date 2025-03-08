(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.structure_from_string s with
  | Ok structure ->
    (match Inferencer.run_structure_infer structure with
     | Ok _ ->
       let structure, varSet = Alpha_conversion.run_alpha_conversion structure in
       let simple_structure = To_simple_ast.convert structure in
       let simple_structure =
         Closure_conversion.run_closure_conversion simple_structure
       in
       let simple_structure, varSet =
         Lambda_lifting.run_lambda_lifting simple_structure varSet
       in
       let anf_res = To_anf.convert simple_structure varSet in
       (match anf_res with
        | Ok anf ->
          let compiled_program = Compiler.generate_ir anf in
          Format.printf "%s" compiled_program
        | Error e -> Format.printf "Anf conversion error: %s" e)
     | Error e -> Format.printf "Infer: %a" Typedtree.pp_error e)
  | Error err -> Format.printf "Parser: %s\n" err
;;

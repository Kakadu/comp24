(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open XaxaML

let () =
  let str = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.run_parser_program str with
  | Result.Error err -> Format.printf "Parsing error%s\n" err
  | Result.Ok parsed ->
    (match Inferencer.run_infer_program parsed with
     | Error err -> Format.printf "Type inference error: %a\n" Inferencer.pp_error err
     | Ok last_typed_env ->
       Format.printf
         "Types before modifications:\n%a"
         Inferencer.TypeEnv.pp_env
         last_typed_env;
       let nh, names_num, ast = Remove_patterns.run parsed in
       let nh, names_num, ast = Alpha_conversion.run nh names_num ast in
       let ast = Closure_conversion.run_closure_conversion_program ast in
       let nh, names_num, ast = Lambda_lifting.run nh names_num ast in
       (match Anf.run_to_anf nh names_num ast with
        | Error err -> Format.printf "Error converting to ANF: %a\n" Anf.PP.pp_error err
        | Ok ast ->
          let restore_ast = Anf.Convert.to_ast ast in
          (match Inferencer.run_infer_program restore_ast with
           | Error err ->
             Format.printf "Type inference error: %a\n" Inferencer.pp_error err
           | Ok new_typed_env ->
             Format.printf
               "\nTypes after modifications:\n%a"
               Inferencer.TypeEnv.pp_env
               new_typed_env);
          Format.printf "\nModified ast:\n%a" Anf.PP.pp_anf_program ast))
;;

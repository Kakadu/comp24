(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open XaxaML

let () =
  let str = Stdio.In_channel.input_all Stdlib.stdin in
  let parsed = Parser.run_parser_program str in
  match parsed with
  | Result.Error err -> Format.printf "Parsing error%s\n" err
  | Result.Ok parsed ->
    (match Inferencer.run_infer_program parsed with
     | Error err -> Format.printf "Type inference error: %a\n" Inferencer.pp_error err
     | Ok last_typed_env ->
       Format.printf
         "Types before modifications:\n%a"
         Inferencer.TypeEnv.pp_env
         last_typed_env;
       let ast = Remove_patterns.run_remove_patterns_program parsed in
       let fresh_number, ast = Alpha_conversion.run_alpha_conversion_program ast in
       let ast = Closure_conversion.run_closure_conversion_program ast in
       let _, ast = Lambda_lifting.run_lambda_lifting_program fresh_number ast in
       let restore_ast = Remove_patterns.ToAst.convert_program ast in
       (match Inferencer.run_infer_program restore_ast with
        | Error err -> Format.printf "Type inference error: %a\n" Inferencer.pp_error err
        | Ok new_typed_env ->
          Format.printf
            "Types after modifications:\n%a"
            Inferencer.TypeEnv.pp_env
            new_typed_env);
       Format.printf "Modified ast:\n%a" Remove_patterns.PP.pp_rp_program ast)
;;

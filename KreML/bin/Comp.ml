(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Kreml_lib.Inferencer

let () =
  let open Stdlib.Format in
  let input = In_channel.input_all stdin in
  match Kreml_lib.Parser.run input with
  | Ok structure ->
    (match Kreml_lib.Inferencer.run structure with
     | Ok env ->
       TypeEnv.pp std_formatter env;
       fprintf std_formatter "applying alpha conversion\n";
       let anf_structure = Kreml_lib.Ast_transformer.transform_structure structure in
       Kreml_lib.Ast_printer.pp_structure std_formatter anf_structure;
       let env_equals env1 env2 =
          let s1 = asprintf "%a" TypeEnv.pp env1 in
          let s2 = asprintf "%a" TypeEnv.pp env2 in
          String.equal s1 s2 in
       (match Kreml_lib.Inferencer.run anf_structure with
       | Ok anf_env when env_equals env anf_env -> ()
       | Ok anf_env ->
         fprintf std_formatter "Type environment changed after applying anf:\n expected: %a\n actual:%a" TypeEnv.pp env TypeEnv.pp anf_env
       | Error e ->
         fprintf std_formatter "An error %a occured while inferencing program in ANF." pp_error e)
     | Error error ->
       fprintf std_formatter "An error occured while type checking: %a" pp_error error)
  | Error _ -> fprintf std_formatter "Could not parse the program %s" input
;;

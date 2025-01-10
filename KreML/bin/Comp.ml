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
       fprintf std_formatter "applying alpha conversion";
       let s = Kreml_lib.Ast_transformer.transform_structure structure in
       Kreml_lib.Ast_printer.pp_structure std_formatter s
     | Error error ->
       fprintf std_formatter "An error occured while type checking: %a" pp_error error)
  | Error _ -> fprintf std_formatter "Could not parse the program %s" input
;;

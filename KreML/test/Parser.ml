(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Kreml_lib.Parser

let () =
  let input = In_channel.input_all Stdlib.stdin in
  let open Stdlib.Format in
  match run input with
  | Ok s ->
    fprintf
      std_formatter
      "%a \n %a"
      Kreml_lib.Ast_printer.pp_structure
      s
      Kreml_lib.Ast.pp_structure
      s
  | Error msg -> fprintf std_formatter "%s" msg
;;

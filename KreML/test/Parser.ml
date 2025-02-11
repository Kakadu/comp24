(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Cursedml_lib.Parser

let () =
  let input = In_channel.input_all Stdlib.stdin in
  let open Stdlib.Format in
  match run input with
  | Ok s ->
    fprintf
      std_formatter
      "%a \n %a"
      Cursedml_lib.Ast_printer.pp_structure
      s
      Cursedml_lib.Ast.pp_structure
      s
  | Error msg -> fprintf std_formatter "%s" msg
;;

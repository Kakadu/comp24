(** Copyright 2023-2024, Efim Perevalov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MEML_lib.Parser

let parsing_and_inerpretation input =
  match parser input with
  | Ok ast -> MEML_lib.Ast.pp_statements Format.std_formatter ast
  | Error e -> Format.printf "Parsing error %s" e
;;

let () = parsing_and_inerpretation (Stdio.In_channel.input_all Stdlib.stdin)
(** Copyright 2024-2025, Perevalov Efim, Ermolovich Anna *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MEML_lib.Parser
open MEML_lib.PprinterAST

let parsing_and_inerpretation input =
  match parser input with
  | Ok ast -> Format.print_string @@ printer ast
  | Error e -> Format.printf "Parsing error %s" e
;;

let () = parsing_and_inerpretation (Stdio.In_channel.input_all Stdlib.stdin)

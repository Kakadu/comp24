(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open XaxaML

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.run_parser_program s with
  | Result.Ok ast -> Format.printf "%a\n" Ast.pp_program ast
  | Error err -> Format.printf "Parsing error%s\n" err
;;

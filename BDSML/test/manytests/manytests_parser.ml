(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Test_parser

let () =
  let str = Stdio.In_channel.input_all Stdlib.stdin in
  Test_utils.pp_result Parser.Ast.pp_structure @@ Parser.Main_parser.parse str
;;

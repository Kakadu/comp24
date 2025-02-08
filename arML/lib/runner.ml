(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Parser runners *)

let parse_program program = Parser.Runner.parse program

let parse_program_with_print program =
  match Parser.Runner.parse program with
  | Ok ast -> Parser.Pprint.print_parser_result ast
  | Error _ -> Parser.Pprint.print_parser_error Parser.Error.Syntax_error
;;

(* -------------- *)

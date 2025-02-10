(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Parser runner *)

let parse_program program = Parser.Runner.parse program

let parse_program_with_print program =
  match Parser.Runner.parse program with
  | Ok ast -> Parser.Pprint.print_parser_result ast
  | Error _ -> Parser.Pprint.print_parser_error Parser.Error.Syntax_error
;;

(* -------------- *)

(* Inferencer runner *)

let inference_expr ast =
  match Inferencer.Runner.run_expr_inferencer ast with
  | Ok typ -> Inferencer.PpType.print_expr_type typ
  | Error e -> Inferencer.PpTypeErrors.print_inferencer_error e
;;

let inference_program ast =
  match Inferencer.Runner.run_program_inferencer ast with
  | Ok (env, names_list) -> Inferencer.PpType.print_program_type env names_list
  | Error e -> Inferencer.PpTypeErrors.print_inferencer_error e
;;

let inference program =
  let ast = parse_program program in
  match ast with
  | Ok [SExpression expr] -> inference_expr expr
  | Ok ast -> inference_program ast
  | Error _ -> Parser.Pprint.print_parser_error Parser.Error.Syntax_error
;;

(* -------------- *)

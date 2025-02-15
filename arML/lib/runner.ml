(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(* Parser runner *)

let parse_expression expr =
  match Parser.Runner.parse_expression expr with
  | Ok [ expr ] -> Parser.PpParsingResult.print_expression_parsing_result expr
  | _ -> Parser.PpParsingError.print_parser_error Parser.Error.Syntax_error
;;

let parse_program program =
  match Parser.Runner.parse_program program with
  | Ok ast -> Parser.PpParsingResult.print_program_parsing_result ast
  | Error _ -> Parser.PpParsingError.print_parser_error Parser.Error.Syntax_error
;;

(* -------------- *)

(* Inferencer runner *)

let inference_expr_ast ast =
  match Inferencer.Runner.run_expr_inferencer ast with
  | Ok typ -> Inferencer.PpType.print_expr_type typ
  | Error e -> Inferencer.PpTypeErrors.print_inferencer_error e
;;

let inference_program_ast ast =
  match Inferencer.Runner.run_program_inferencer ast with
  | Ok (env, names_list) -> Inferencer.PpType.print_program_type env names_list
  | Error e -> Inferencer.PpTypeErrors.print_inferencer_error e
;;

let inference_expression expr =
  let ast = Parser.Runner.parse_expression expr in
  match ast with
  | Ok [ expr ] -> inference_expr_ast expr
  | _ -> Parser.PpParsingError.print_parser_error Parser.Error.Syntax_error
;;

let inference_program program =
  let ast = Parser.Runner.parse_program program in
  match ast with
  | Ok decl_list -> inference_program_ast decl_list
  | Error _ -> Parser.PpParsingError.print_parser_error Parser.Error.Syntax_error
;;

(* -------------- *)

(* Closure conversion *)

let closure_conversion_expr_ast expr =
  Ast.Pprint.print_expression (Middleend.Runner.run_closure_expression expr)
;;

let closure_conversion_program_ast program =
  Ast.Pprint.print_program (Middleend.Runner.run_closure_program program)
;;

let closure_conversion_expression expr =
  match Parser.Runner.parse_expression expr with
  | Ok [ ast ] -> closure_conversion_expr_ast ast
  | _ -> Parser.PpParsingError.print_parser_error Parser.Error.Syntax_error
;;

let closure_conversion_program program =
  match Parser.Runner.parse_program program with
  | Ok ast -> closure_conversion_program_ast ast
  | Error _ -> Parser.PpParsingError.print_parser_error Parser.Error.Syntax_error
;;

(* -------------- *)

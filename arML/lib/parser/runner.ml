(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Common
open Expression
open Declaration
open Angstrom
open Pprint
open Error

let parsers = 
  { parse_type_defition
  ; parse_constant_expr
  ; parse_identifier_expr
  ; parse_fun
  ; parse_application
  ; parse_binary_operation
  ; parse_if_then_else
  ; parse_let_in
  ; parse_match_with
  ; parse_function
  ; parse_tuple
  ; parse_list
  ; parse_empty_list_expr
  }
;;

let parse_expression = skip_wspace *> choice 
  [ parsers.parse_type_defition parsers
  ; parsers.parse_binary_operation parsers
  ; parsers.parse_list parsers
  ; parsers.parse_application parsers
  ; parsers.parse_tuple parsers
  ; parsers.parse_constant_expr
  ; parsers.parse_identifier_expr
  ; parsers.parse_fun parsers
  ; parsers.parse_function parsers
  ; parsers.parse_if_then_else parsers
  ; parsers.parse_let_in parsers
  ; parsers.parse_match_with parsers
  ; parsers.parse_empty_list_expr
  ] <* skip_wspace
;;

let expression_parser =
  let* expr = parse_expression in
  return @@ SExpression expr
;;

let declaration_parser =
  let* decl = parse_declaration parsers in
  return @@ SDeclaration decl
;;

let parse input =
  parse_string ~consume:All (many1 (expression_parser <|> declaration_parser)) input
;;

let parse_with_print code =
  (* The 'parse' function converts the result to a string.
     Used in tests. *)
  match parse code with
  | Ok ast -> print_parser_result ast
  | Error _ -> print_parser_error Syntax_error
;;

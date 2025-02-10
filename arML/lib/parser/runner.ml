(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Common
open Expression
open Declaration
open Angstrom

let expression_parser =
  let* expr = parse_expression in
  return @@ SExpression expr
;;

let declaration_parser =
  let* decl = parse_declaration parsers in
  return @@ SDeclaration decl
;;

let parse input =
  let sep = option () (skip_wspace *> string ";;" *> return ()) in
  parse_string ~consume:All (many1 ((expression_parser <|> declaration_parser) <* sep <* skip_wspace)) input
;;

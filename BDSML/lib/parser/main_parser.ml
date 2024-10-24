(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Angstrom
open Ast
open Utils

let parse_str_value =
  let+ rec_flag, bindings = Expr_parser.parse_let_main_part Expr_parser.parse_expr in
  Str_value (rec_flag, bindings)
;;

let parse_str_eval =
  let+ res = Expr_parser.parse_expr in
  Str_eval res
;;

let semicolon_opt = many (ws *> string ";;")
let parse_structure = many (parse_str_value <|> parse_str_eval <* semicolon_opt) <* ws
let parse str = Angstrom.parse_string ~consume:Angstrom.Consume.All parse_structure str

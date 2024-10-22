(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Angstrom
open Ast
open Utils

(** https://ocaml.org/manual/5.2/lex.html#sss:lex-ops-symbols *)
let is_core_operator_char = function
  | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '>' | '@' | '^' | '|' -> true
  | _ -> false
;;

let is_operator_char a =
  is_core_operator_char a
  ||
  match a with
  | '~' | '!' | '?' | '%' | '<' | ':' | '.' -> true
  | _ -> false
;;

let parse_prefix_op =
  let case1 =
    let* first_el = char '?' <|> char '~' in
    let+ other_el = take_while1 is_operator_char in
    Char.escaped first_el ^ other_el
  in
  let case2 =
    let* first_el = char '!' in
    let+ other_el = take_while is_operator_char in
    Char.escaped first_el ^ other_el
  in
  case1 <|> case2
;;

let parse_infix_op prefix =
  let case1 =
    let+ first_el = char '#'
    and+ other_el = take_while1 is_operator_char in
    Char.escaped first_el ^ other_el
  in
  let case2 =
    let+ first_el = satisfy is_core_operator_char <|> char '%' <|> char '<'
    and+ other_el = take_while is_operator_char in
    Char.escaped first_el ^ other_el
  in
  let* check_prefix = peek_string @@ String.length prefix in
  if String.equal check_prefix prefix then case1 <|> case2 else fail ""
;;

type associativity =
  | Left
  | Right

type op_type =
  | Prefix
  | Infix of associativity

(** https://ocaml.org/manual/5.2/expr.html#ss%3Aprecedence-and-associativity
    by priority from higher to lower*)
let operators =
  let parse_infix_with_prefixes prefixes = choice (List.map parse_infix_op prefixes) in
  [ Prefix, parse_prefix_op
  ; Infix Left, parse_infix_op "#"
  ; Prefix, string "-." <|> string "-"
  ; Infix Right, parse_infix_op "**"
  ; Infix Left, parse_infix_with_prefixes [ "*"; "/"; "%" ]
  ; Infix Left, parse_infix_with_prefixes [ "+"; "-" ]
  ; Infix Right, string "::"
  ; Infix Right, parse_infix_with_prefixes [ "@"; "^" ]
  ; Infix Left, parse_infix_with_prefixes [ "="; "<"; ">"; "|"; "&"; "$" ] <|> string "!="
  ; Infix Right, string "&" <|> string "&&"
  ; Infix Right, string "or" <|> string "||"
  ; Infix Right, string "<-" <|> string ":="
  ; Infix Right, string ";"
  ]
;;

let parse_ident =
  let+ ident = parse_lowercase_ident in
  Exp_ident ident
;;

let parse_const =
  let+ const = Const_parser.parse_const in
  Exp_constant const
;;

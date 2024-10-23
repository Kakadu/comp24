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
  let+ ident = ws *> parse_lowercase_ident in
  Exp_ident ident
;;

let parse_const =
  let+ const = Const_parser.parse_const in
  Exp_constant const
;;

let rec unary_chain (func : string t) arg_parser =
  let* parsed_func = ws *> func in
  let+ arg = unary_chain func arg_parser <|> arg_parser in
  let ident = Exp_ident parsed_func in
  Exp_apply (ident, arg)
;;

let parse_expr =
  let parse_bop (op : string t) =
    let+ parsed = ws *> op in
    let ident = Exp_ident parsed in
    fun a b -> Exp_apply (ident, Exp_tuple [ a; b ])
  in
  let op_iter prev = function
    | Infix Right, el -> chainr1 prev (parse_bop el)
    | Infix Left, el -> chainl1 prev (parse_bop el)
    | Prefix, el -> unary_chain el prev <|> prev
  in
  let rec parse_exp_ prev = function
    | h :: tl -> parse_exp_ (op_iter prev h) tl
    | [] -> prev
  in
  let init = parse_const <|> parse_ident in
  parse_exp_ init operators
;;

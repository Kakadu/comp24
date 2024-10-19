(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Angstrom
open Ast
open Utils

type associativity = Left | Right | Unspec
type args = Unary | Binary

type op_data =  {
  args : args;
  assoc : associativity;
  priority : int;
}

(**https://ocaml.org/manual/5.2/api/Ocaml_operators.html*)
let get_prefix_symbol_op_data_next_need = function
| '~'-> Some({args = Unary; assoc = Unspec priority = 20})
| '#'-> Some({args = Binary; assoc = Left priority = 18})
| _ -> None

let get_prefix_symbol_op_data_next_unneed = function
| '!'-> Some({args = Unary; assoc = Unspec; priority = 20})
| '*'-> Some({args = Binary; assoc = Left, priority =14})
| '/'-> Some({args = Binary; assoc = Left, priority =14})
| '%'-> Some({args = Binary; assoc = Left, priority =14})
| '+'-> Some({args = Binary; assoc = Left, priority =13})
| '-'-> Some({args = Binary; assoc = Left, priority =13})
| '@'-> Some({args = Binary; assoc = Right priority =11})
| '^'-> Some({args = Binary; assoc = Right priority =11})
| '='-> Some({args = Binary; assoc = Left, priority =10})
| '<'-> Some({args = Binary; assoc = Left, priority =10})
| '>'-> Some({args = Binary; assoc = Left, priority =10})
| '|'-> Some({args = Binary; assoc = Left, priority =10})
| '&'-> Some({args = Binary; assoc = Left, priority =10})
| '$'-> Some({args = Binary; assoc = Left, priority =10})
| _ -> None

let get_prefix_two_symbols_op_data = function
| "**" -> Some({args = Binary; assoc = Right, priority =15})
| ':' -> Some({args = Binary; assoc = Right, priority =12})
| _ -> None

let get_single_op_data = function
| "-" -> Some({args = Binary; assoc = Unspec; priority = 16})
| "-." -> Some({args = Binary; assoc = Unspec; priority = 16})
| "::" -> Some({args = Binary; assoc = Right, priority =12})
| "!=" -> Some({args = Binary; assoc = Left, priority =10})
| "&" -> Some({args = Binary; assoc = Right, priority =9})
| "&&" -> Some({args = Binary; assoc = Right, priority =9})
| "or" -> Some({args = Binary; assoc = Right, priority =8})
| "||" -> Some({args = Binary; assoc = Right, priority =8})
| "," -> Some({args = Binary; assoc = Unspec, priority =7})
| "<-" -> Some({args = Binary; assoc = Right, priority =6})
| ":=" -> Some({args = Binary; assoc = Right, priority =6})
| ";" -> Some({args = Binary; assoc = Right, priority =4})
| _ -> None

let get_operator_data = 
let prefix_op =
  let is_first_char_need_following = function
    | '?' | '~' -> true
    | _ -> false
  in
  let is_first_char_unneed_following = function
    | '!' -> true
    | _ -> false
  in
  let is_next_char = function
    | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '>' | '@' | '^' | '|' | '%' | '<' -> true
    | _ -> false
  in
  let case1 =
    let* first_el = satisfy is_first_char_need_following in
    let+ other_el = take_while1 is_next_char in
    Char.escaped first_el ^ other_el
  in
  let case2 =
    let* first_el = satisfy is_first_char_unneed_following in
    let+ other_el = take_while is_next_char in
    Char.escaped first_el ^ other_el
  in
  case1 <|> case2
;;

let binary_op =
  let is_first_char_need_following = function
    | '#' -> true
    | _ -> false
  in
  let is_first_char_unneed_following = function
    | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '>' | '@' | '^' | '|' | '%' | '<' -> true
    | _ -> false
  in
  let is_next_char = function
    | '$'
    | '&'
    | '*'
    | '+'
    | '-'
    | '/'
    | '='
    | '>'
    | '@'
    | '^'
    | '|'
    | '%'
    | '<'
    | '!'
    | '.'
    | ':'
    | '?'
    | '~' -> true
    | _ -> false
  in
  let case1 =
    let* first_el = satisfy is_first_char_need_following in
    let+ other_el = take_while1 is_next_char in
    Char.escaped first_el ^ other_el
  in
  let case2 =
    let* first_el = satisfy is_first_char_unneed_following in
    let+ other_el = take_while is_next_char in
    Char.escaped first_el ^ other_el
  in
  case1 <|> case2
;;

let a = and+ char 'a' in char 'b'

let parse_ident =
  let+ ident = parse_lowercase_ident in
  Exp_ident ident
;;

let parse_const =
  let+ const = Const_parser.parse_const in
  Exp_constant const
;;

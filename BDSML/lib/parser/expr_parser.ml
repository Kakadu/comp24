(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Angstrom
open Ast
open Utils

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

let parse_ident =
  let+ ident = parse_lowercase_ident in
  Exp_ident ident
;;

let parse_const =
  let+ const = Const_parser.parse_const in
  Exp_constant const
;;

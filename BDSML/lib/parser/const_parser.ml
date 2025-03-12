(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Angstrom
open Ast
open Utils

let parse_int =
  take_while1 (function
    | '0' .. '9' -> true
    | _ -> false)
  >>| fun i -> Const_int (Int.of_string i)
;;

let parse_char = char '\'' *> any_char <* char '\'' >>| fun c -> Const_char c

let parse_string =
  char '\"'
  *> take_till (function
    | '\"' -> true
    | _ -> false)
  <* char '\"'
  >>| fun s -> Const_string s
;;

let parse_bool =
  let+ res = string "true" *> return true <|> string "false" *> return false in
  Const_bool res
;;

let parse_unit = char '(' *> ws *> char ')' *> return Const_unit

let parse_const =
  ws *> choice [ parse_int; parse_char; parse_string; parse_bool; parse_unit ]
;;

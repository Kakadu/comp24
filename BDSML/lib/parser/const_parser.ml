(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Angstrom
open Ast

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

let parse_const = choice [ parse_int; parse_char; parse_string ]

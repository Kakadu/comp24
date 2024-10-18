(** Copyright 2023-2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Ast
open Angstrom
open Utils

let parse_single = parse_ident_name >>| fun id -> Type_single id

let parse_tuple s =
  sep_by1 (char '*' <* ws) s
  >>= fun t_list ->
  if List.length t_list >= 2
  then return @@ Type_tuple t_list
  else fail "tuple of types length must be >= 2"
;;

let parse_fun s =
  sep_by1 (char '*' <* ws) s
  >>= fun t_list ->
  if List.length t_list >= 2
  then return @@ Type_tuple t_list
  else fail "types of fun length must be >= 2"
;;

let parse_type = fix (fun s -> parse_single <|> parse_tuple s <|> parse_fun s)

let parse_ident =
  let* name = parse_ident_name in
  let+ id_type = char ':' *> ws *> parse_type >>| Option.some <|> return None in
  { name; id_type }
;;

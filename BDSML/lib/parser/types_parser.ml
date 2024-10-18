(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Ast
open Angstrom
open Utils

let parse_single =
  let+ id = parse_ident_name in
  Type_single id
;;

let parse_tuple s =
  let* t_list = sep_by1 (char '*' <* ws) s in
  if List.length t_list >= 2
  then return @@ Type_tuple t_list
  else fail "tuple of types length must be >= 2"
;;

let parse_fun s =
  let* t_list = sep_by1 (char '*' <* ws) s in
  if List.length t_list >= 2
  then return @@ Type_tuple t_list
  else fail "types of fun length must be >= 2"
;;

let parse_params s =
  let* type_param = s <* ws1 in
  let+ main_type = parse_ident_name in
  Type_params (type_param, main_type)
;;

let parse_type =
  fix (fun s -> rec_remove_parents @@ parse_single <|> parse_tuple s <|> parse_fun s)
;;

let parse_ident =
  let* name = parse_ident_name in
  let+ id_type = char ':' *> ws *> parse_type >>| Option.some <|> return None in
  { name; id_type }
;;

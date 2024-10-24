(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Ast
open Angstrom
open Utils

(** [int] *)
let parse_single =
  let+ id = parse_ident_name in
  Type_single id
;;

(** [int t list] *)
let parse_typeconstr prev =
  let rec go acc =
    (let+ main_type = ws1 *> parse_ident_name in
     Type_params (acc, main_type))
    >>= go
    <|> return acc
  in
  prev >>= go
;;

(** [int * string * int] *)
let parse_tuple prev =
  sep_by1 (check_char '*') prev
  >>= function
  | _ :: _ :: _ as t_list -> return @@ Type_tuple t_list
  | [ t ] -> return t
  | _ -> fail "invalid state in tuple type"
;;

(** [int -> int] *)
let parse_fun prev =
  sep_by1 (check_string "->") prev
  >>= function
  | _ :: _ :: _ as t_list -> return @@ Type_fun t_list
  | [ t ] -> return t
  | _ -> fail "invalid state in function type"
;;

(** https://ocaml.org/manual/5.2/types.html#typexpr *)
let priority = [ parse_typeconstr; parse_tuple; parse_fun ]

let parse_typexpr_by_prior =
  fix (fun self ->
    parse_by_priority priority @@ choice [ parse_single; remove_parents self ])
;;

(** for [fun pat: typexpr -> expr] parse *)
let parse_skip_fun =
  check_char ':'
  *> (parse_by_priority [ parse_typeconstr; parse_tuple ]
      @@ choice [ parse_single; remove_parents parse_typexpr_by_prior ])
;;

(** [:int] *)
let parse_typexpr : typexpr t = check_char ':' *> parse_typexpr_by_prior

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(* Expression parsers description *)

type dispatch =
  { parse_tuple : dispatch -> expression Angstrom.t
  ; parse_list : dispatch -> expression Angstrom.t
  ; parse_list_constructor : dispatch -> expression Angstrom.t
  ; parse_fun : dispatch -> expression Angstrom.t
  ; parse_function : dispatch -> expression Angstrom.t
  ; parse_application : dispatch -> expression Angstrom.t
  ; parse_binary_operation : dispatch -> expression Angstrom.t
  ; parse_match_with : dispatch -> expression Angstrom.t
  ; parse_let_in : dispatch -> expression Angstrom.t
  ; parse_if_then_else : dispatch -> expression Angstrom.t
  ; parse_type_defition : dispatch -> expression Angstrom.t
  ; parse_constant_expr: expression Angstrom.t
  ; parse_identifier_expr: expression Angstrom.t
  ; parse_empty_list_expr: expression Angstrom.t
  }

(* ---------------- *)

(* Helper functions *)

val is_whitespace : char -> bool
val is_upper_char : char -> bool
val is_lower_char : char -> bool
val is_digit_char : char -> bool
val is_acceptable_service_char : char -> bool
val is_operator_char : char -> bool
val is_keyword : string -> bool
val chainl1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t
val chainr1 : 'a Angstrom.t -> ('a -> 'a -> 'a) Angstrom.t -> 'a Angstrom.t

(* ---------------- *)

(* Helper parsers *)

val skip_wspace : unit Angstrom.t
val skip_wspace1 : string Angstrom.t

val parens : 'a Angstrom.t -> 'a Angstrom.t
val brackets : 'a Angstrom.t -> 'a Angstrom.t

val arrow : string Angstrom.t
val trait : string Angstrom.t
val tying : string Angstrom.t

(* ---------------- *)

(* Constant parsers *)

val parse_constant : constant Angstrom.t

(* ---------------- *)

(* Identifiers *)

val parse_name : string Angstrom.t
val parse_uncapitalized_name : string Angstrom.t
val parse_capitalized_name : string Angstrom.t
val parse_identifier : identifier Angstrom.t
val parse_operator : identifier Angstrom.t

(* ---------------- *)

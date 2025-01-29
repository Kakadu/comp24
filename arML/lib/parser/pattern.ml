(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Angstrom
open Common
open Type

(* Pattern parsers description *)

type pattern_dispatch = 
  { parse_tuple_pattern: pattern_dispatch -> pattern Angstrom.t
  ; parse_list_pattern : pattern_dispatch -> pattern Angstrom.t
  ; parse_or_pattern : pattern_dispatch -> pattern Angstrom.t
  ; parse_constant_pattern: pattern Angstrom.t
  ; parse_identifier_pattern: pattern Angstrom.t
  ; parse_nill_pattern: pattern Angstrom.t
  ; parse_any_pattern: pattern Angstrom.t
  ; parse_pattern_type_defition: pattern_dispatch -> pattern Angstrom.t
  }

(* ---------------- *)

let parse_constant_pattern =
  let* constant = parse_constant in
  return @@ PConst constant

(* ---------------- *)

(* Identifiers pattern parsers *)

let parse_identifier_pattern =
  let* identifier = parse_identifier in
  return @@ PVar identifier

(* ---------------- *)

(* Nil pattern parsers *)

let parse_nill_pattern = 
  let* _ = brackets (string "") in
  return PNill

(* ---------------- *)

(* Wildcard pattern parsers *)

let parse_any_pattern = 
  let* _ = skip_wspace *> char '_' in
  return PAny
;;

(* ---------------- *)

(* Alternative (Or) pattern parser *)

let parse_or_pattern pp =
  fix
  @@ fun self ->
  let parse_left_pattern = choice
    [ pp.parse_list_pattern pp
    ; pp.parse_tuple_pattern pp
    ; pp.parse_constant_pattern
    ; pp.parse_identifier_pattern
    ; pp.parse_nill_pattern
    ; pp.parse_any_pattern
    ; pp.parse_pattern_type_defition pp
    ; parens self
    ]
  in

  let parse_right_pattern = choice 
    [ self
    ; parse_left_pattern
    ]
  in

  let* left_pattern = parse_left_pattern in
  let* _ = skip_wspace *> char '|' <* skip_wspace in
  let* right_pattern = parse_right_pattern in
  return @@ POr (left_pattern, right_pattern)
;;

(* ---------------- *)

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

(* Tuple pattern parsers *)

let parse_tuple_pattern pp =
  fix
  @@ fun self ->
  let parse_pattern = choice 
    [ pp.parse_pattern_type_defition pp
    ; pp.parse_or_pattern pp
    ; pp.parse_list_pattern pp
    ; pp.parse_constant_pattern
    ; pp.parse_identifier_pattern
    ; pp.parse_nill_pattern
    ; pp.parse_any_pattern
    ; self
    ]
  in
  let* patterns = parens (sep_by (skip_wspace *> char ',' <* skip_wspace) parse_pattern) in
  match patterns with
  | pat_1 :: pat_2 :: pats -> return (PTuple (pat_1, pat_2, pats))
  | _ -> fail "Syntax error: tuple must have at least two patterns"
;;

(* ---------------- *)

(* List pattern parser *)

let parse_list_pattern pp =
  fix
  @@ fun self ->
  let parse_pattern = choice
    [ pp.parse_pattern_type_defition pp
    ; parens @@ pp.parse_or_pattern pp
    ; pp.parse_constant_pattern
    ; pp.parse_identifier_pattern
    ; pp.parse_nill_pattern
    ; pp.parse_any_pattern
    ; pp.parse_tuple_pattern pp
    ; parens @@ self
    ]
  in

  let parse_list_cons =
    let parse_operator =
      skip_wspace *> string "::" <* skip_wspace
      >>| fun _ head tail -> PListConstructor (head, tail)
    in
    chainr1 parse_pattern parse_operator
  in

  let parse_list_syntax =
    brackets (sep_by (skip_wspace *> char ';' <* skip_wspace) parse_pattern) >>= function
    | [] -> return PNill
    | head :: tail ->
      List.fold_right (fun pat acc -> PListConstructor (pat, acc)) tail (PListConstructor (head, PNill))
      |> return
  in

  choice [parse_list_cons; parse_list_syntax]
;;

(* ---------------- *)

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Angstrom
open Common

(* Constant pattern parsers *)

let parse_constant_pattern =
  let* constant = parse_constant in
  return @@ PConst constant
;;

(* ---------------- *)

(* Identifiers pattern parsers *)

let parse_identifier_pattern =
  let* identifier = parse_identifier <|> parse_operator in
  return @@ PVar identifier
;;

(* ---------------- *)

(* Nil pattern parsers *)

let parse_nil_pattern =
  let* _ = brackets (string "") in
  return PNill
;;

(* ---------------- *)

(* Wildcard pattern parsers *)

let parse_any_pattern =
  let* _ = skip_wspace *> char '_' in
  return PAny
;;

(* ---------------- *)

(* Tuple pattern parsers *)

let parse_tuple_pattern pp =
  parens
  @@
  let* patterns = sep_by (skip_wspace *> char ',' <* skip_wspace) pp in
  match patterns with
  | p1 :: p2 :: ps -> return (PTuple (p1, p2, ps))
  | _ -> fail "Syntax error: tuple must have at least two patterns"
;;

(* ---------------- *)

(* List pattern parser *)

let parse_list_pattern pp =
  let main_parser = brackets @@ sep_by (skip_wspace *> char ';' <* skip_wspace) pp in
  let rec constructor_helper = function
    | [] -> PNill
    | hd :: tl -> PListConstructor (hd, constructor_helper tl)
  in
  let* patterns = main_parser in
  return @@ constructor_helper patterns
;;

let parse_list_constructor_pattern pp =
  let constructor hd tl = PListConstructor (hd, tl) in
  let parse_constructor =
    skip_wspace *> string "::" *> skip_wspace *> return constructor
  in
  chainr1 pp parse_constructor
;;

(* ---------------- *)

(* Pattern's type definition parser *)

let parse_typed_pattern pp =
  parens
  @@
  let* pat = pp in
  let* typ = skip_wspace *> char ':' *> Type.parse_type in
  return @@ PTyped (pat, typ)
;;

(* ---------------- *)

(* Main pattern parser *)

let parse_pattern =
  fix
  @@ fun self ->
  let basic_pattern_parsers =
    choice
      [ parse_constant_pattern
      ; parse_identifier_pattern
      ; parse_any_pattern
      ; parse_nil_pattern
      ]
  in
  let composite_pattern_parsers =
    choice
      [ basic_pattern_parsers
      ; parens self
      ; parse_list_pattern self
      ; parse_typed_pattern self
      ; parse_tuple_pattern self
      ]
  in
  let chain_pattern_parsers =
    choice
      [ parse_list_constructor_pattern composite_pattern_parsers
      ; composite_pattern_parsers
      ]
  in
  chain_pattern_parsers
;;

(* ---------------- *)

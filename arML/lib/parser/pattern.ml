(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Angstrom
open Common
open Type

(* Pattern parsers description *)

type pattern_dispatch = 
  { parse_tuple_pattern: pattern_dispatch -> pattern Angstrom.t
  ; parse_list_pattern : pattern_dispatch -> pattern Angstrom.t
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
  let* identifier = parse_identifier <|> parse_operator in
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

(* Tuple pattern parsers *)

let parse_tuple_pattern pp =
  fix
  @@ fun self ->
  let parse_pattern = choice 
      [ pp.parse_pattern_type_defition pp
      ; pp.parse_list_pattern pp
      ; pp.parse_constant_pattern
      ; pp.parse_identifier_pattern
      ; pp.parse_nill_pattern
      ; pp.parse_any_pattern
      ; parens self
      ]
  in
  let* patterns = (sep_by (skip_wspace *> char ',' <* skip_wspace) parse_pattern) in
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
      [ pp.parse_constant_pattern
      ; pp.parse_identifier_pattern
      ; pp.parse_nill_pattern
      ; pp.parse_any_pattern
      ; parens @@ pp.parse_pattern_type_defition pp
      ; parens @@ pp.parse_tuple_pattern pp
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

(* Pattern's type definition parser *)

let parse_pattern_type_defition pp =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_pattern = choice 
      [ pp.parse_constant_pattern
      ; pp.parse_identifier_pattern
      ; pp.parse_nill_pattern
      ; pp.parse_any_pattern
      ; parens @@ pp.parse_list_pattern pp
      ; parens @@ pp.parse_tuple_pattern pp
      ; parens @@ self
      ]
  in
  parens @@
  let* pat = parse_pattern in
  let* typ = skip_wspace *> char ':' *> parse_type in
  return @@ PTyped (pat, typ)
;;

(* ---------------- *)

(* Main pattern parser *)

let parsers =
  { parse_constant_pattern
  ; parse_identifier_pattern
  ; parse_nill_pattern
  ; parse_any_pattern
  ; parse_tuple_pattern
  ; parse_list_pattern
  ; parse_pattern_type_defition
  }

let parse_pattern fun_flag =
  (* If first argument is true then can parse without brackets around the pattern. 
     Otherwise, parentheses are required *)
  let between = if fun_flag then parens else (fun x -> x) in
  choice 
    [ between @@ parsers.parse_tuple_pattern parsers
    ; between @@ parsers.parse_list_pattern parsers
    ; parsers.parse_constant_pattern
    ; parsers.parse_identifier_pattern
    ; parsers.parse_nill_pattern
    ; parsers.parse_any_pattern
    ; parsers.parse_pattern_type_defition parsers
    ]

(* ---------------- *)

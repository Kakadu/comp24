(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Angstrom
open Common

(* Type definition dispatch *)

type type_dispatch =
  { parse_ground_type : type_defenition Angstrom.t
  ; parse_polymorphic_type : type_defenition Angstrom.t
  ; parse_tuple_type : type_dispatch -> type_defenition Angstrom.t
  ; parse_list_type : type_dispatch -> type_defenition Angstrom.t
  ; parse_arrow_type : type_dispatch -> type_defenition Angstrom.t
  }

(* ---------------- *)

(* Ground type definition parser *)

let parse_type =
  skip_wspace
  *>
  let parse_int = string "int" *> return GTDInt in
  let parse_bool = string "bool" *> return GTDBool in
  let parse_unit = string "unit" *> return GTDUnit in
  let parse_char = string "char" *> return GTDChar in
  let parse_string = string "string" *> return GTDString in
  choice [ parse_int; parse_bool; parse_unit; parse_char; parse_string ]
;;

let parse_ground_type =
  fix
  @@ fun self ->
  parens self
  <|>
  let* typ = parse_type in
  return @@ TDGround typ
;;

(* ---------------- *)

(* Tuple type definition parser *)

let parse_tuple_type pt =
  fix
  @@ fun self ->
  skip_wspace *> parens self
  <|>
  let parse_type =
    choice
      [ (* pt.parse_list_type pt *)
        pt.parse_ground_type
      ; pt.parse_polymorphic_type
      ; parens @@ self
      ]
  in
  let main_parser = sep_by (skip_wspace *> string "*" <* skip_wspace) parse_type in
  let* elements = main_parser in
  match elements with
  | pat_1 :: pat_2 :: pats -> return (TDTuple (pat_1, pat_2, pats))
  | _ -> fail "Syntax error: tuple type must have at least two elements"
;;

(* ---------------- *)

(* List type definition parser *)

let parse_list_type pt =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_type =
    choice
      [ pt.parse_ground_type
      ; pt.parse_polymorphic_type
      ; parens @@ self
      ; parens @@ pt.parse_tuple_type pt
      ]
  in
  let parse_type = parens parse_type <|> parse_type in
  let* typ = parse_type in
  let rec parse_list typ =
    let* _ = skip_wspace1 *> string "list" in
    let* next_typ = choice [ parse_list typ; return typ ] in
    return (TDList next_typ)
  in
  parse_list typ
;;

(* ---------------- *)

(* Polymorphic type definition parser *)

let parse_polymorphic_type =
  fix
  @@ fun self ->
  parens self
  <|>
  let* d =
    skip_wspace *> char '\'' *> (parse_uncapitalized_name <|> parse_capitalized_name)
  in
  return @@ TDPolymorphic (Id ("\'" ^ d))
;;

(* ---------------- *)

(* Functional type definition parser *)

let parse_arrow_type pt =
  fix
  @@ fun self ->
  let parse_arrow_operator =
    skip_wspace *> string "->" *> skip_wspace *> return (fun t1 t2 -> TDArrow (t1, t2))
  in
  let parse_simple_type =
    choice
      [ pt.parse_list_type pt
      ; pt.parse_tuple_type pt
      ; pt.parse_ground_type
      ; pt.parse_polymorphic_type
      ]
  in
  let parse_type_with_parens = choice [ parens self; parse_simple_type ] in
  chainr1 parse_type_with_parens parse_arrow_operator
;;

(* ---------------- *)

(* Main type defition parser *)

let parsers =
  { parse_ground_type
  ; parse_tuple_type
  ; parse_list_type
  ; parse_polymorphic_type
  ; parse_arrow_type
  }
;;

let parse_type =
  choice
    [ parsers.parse_list_type parsers
    ; parsers.parse_arrow_type parsers
    ; parsers.parse_tuple_type parsers
    ; parsers.parse_ground_type
    ; parsers.parse_polymorphic_type
    ]
;;

(* ---------------- *)

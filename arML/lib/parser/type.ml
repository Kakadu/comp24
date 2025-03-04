(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Angstrom
open Common

(* Ground type definition parser *)

let parse_ground_type =
  skip_wspace
  *>
  let parse_int = string "int" *> return GTDInt in
  let parse_bool = string "bool" *> return GTDBool in
  let parse_unit = string "unit" *> return GTDUnit in
  let parse_char = string "char" *> return GTDChar in
  let parse_string = string "string" *> return GTDString in
  let* typ = choice [ parse_int; parse_bool; parse_unit; parse_char; parse_string ] in
  return @@ TDGround typ
;;

(* ---------------- *)

(* Polymorphic type definition parser *)

let parse_polymorphic_type =
  skip_wspace
  *>
  let name_parsers = parse_uncapitalized_name <|> parse_capitalized_name in
  let* name = skip_wspace *> char '\'' *> name_parsers in
  return @@ TDPolymorphic (Id ("\'" ^ name))
;;

(* ---------------- *)

(* Tuple type definition parser *)

let parse_tuple_type pt =
  skip_wspace
  *>
  let* elements_typ = sep_by (skip_wspace *> string "*" <* skip_wspace) pt in
  match elements_typ with
  | p1 :: p2 :: ps -> return (TDTuple (p1, p2, ps))
  | _ -> fail "Syntax error: tuple type must have at least two elements"
;;

(* ---------------- *)

(* List type definition parser *)

let parse_list_type pt =
  skip_wspace
  *>
  let* typ = pt in
  let rec parse_list_annotation typ =
    let* _ = skip_wspace1 *> string "list" in
    let* next_typ = choice [ parse_list_annotation typ; return typ ] in
    return (TDList next_typ)
  in
  parse_list_annotation typ
;;

(* ---------------- *)

(* Functional type definition parser *)

let parse_arrow_type pt =
  let constructor p b = TDArrow (p, b) in
  let simple_arrow_parser constr = skip_wspace *> arrow *> skip_wspace *> return constr in
  chainr1 pt (simple_arrow_parser constructor)
;;

(* ---------------- *)

(* Main type defition parser *)

let parse_type =
  fix
  @@ fun self ->
  let p_parens = parens self in
  let p_ground_type = choice [ parse_ground_type; p_parens ] in
  let p_polymorphic_type = choice [ parse_polymorphic_type; p_ground_type ] in
  let p_list_type = choice [ parse_list_type p_polymorphic_type; p_polymorphic_type ] in
  let p_tuple_type = choice [ parse_tuple_type p_list_type; p_list_type ] in
  let p_arrow_type = choice [ parse_arrow_type p_tuple_type; p_tuple_type ] in
  let main_parser = p_arrow_type in
  main_parser
;;

(* ---------------- *)

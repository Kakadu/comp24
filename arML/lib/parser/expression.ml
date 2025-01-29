(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Common

(* Constant expression parsers *)

let parse_constant_expr =
  let* constant = parse_constant in
  return @@ EConstant constant

(* ---------------- *)

(* Identifiers expression parsers *)

let parse_identifier_expr =
  let* identifier = parse_identifier in
  return @@ EIdentifier identifier

(* ---------------- *)

(* Empty list parsers *)

let parse_empty_list_expr =
  let* _ = skip_wspace *> brackets skip_wspace in
  return EEmptyList

(* ---------------- *)

(* Tuple parsers *)

let parse_tuple p =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice
      [ p.parse_type_defition p
      ; p.parse_binary_operation p
      ; p.parse_application p
      ; p.parse_unary_operation p
      ; p.parse_fun p
      ; p.parse_function p
      ; p.parse_let_in p
      ; p.parse_match_with p
      ; p.parse_if_then_else p
      ; parse_constant_expr
      ; parse_identifier_expr
      ; parens @@ self
      ; p.parse_empty_list_expr
      ]
  in

  let main_parser = (sep_by (skip_wspace *> char ',' <* skip_wspace) parse_expr) in

  let* elements = parens main_parser <|> main_parser in

  match elements with
  | pat_1 :: pat_2 :: pats -> return (ETuple (pat_1, pat_2, pats))
  | _ -> fail "Syntax error: tuple must have at least two elements"
;;

(* ---------------- *)

(* If then else parser *)

let parse_if_then_else p =
  fix
  @@ fun self ->
  skip_wspace
  *>
  let parse_expr =
    choice
      [ p.parse_type_defition p
      ; p.parse_binary_operation p
      ; p.parse_unary_operation p
      ; p.parse_application p
      ; p.parse_tuple p
      ; p.parse_fun p
      ; p.parse_let_in p
      ; self
      ; p.parse_constant_expr
      ; p.parse_identifier_expr
      ; p.parse_function p
      ; p.parse_match_with p
      ; p.parse_empty_list_expr
      ]
  in

  parens self
  <|> 
  let* cond = string "if" *> parse_expr in

  let* then_branch = skip_wspace *> string "then" *> parse_expr in

  let opt p = option None (p >>| Option.some) in

  let* else_branch = opt (skip_wspace *> string "else") 
      >>= function
      | Some _ -> skip_wspace *> parse_expr >>| Option.some
      | None -> return None
  in

  return @@ EIfThenElse (cond, then_branch, else_branch)
;;

(* ---------------- *)

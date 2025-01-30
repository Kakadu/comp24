(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast
open Common
open Pattern
open Type

let parse_declaration p =
  let parse_expr =
    choice 
      [ p.parse_binary_operation p
      ; p.parse_type_defition p
      ; p.parse_application p
      ; p.parse_tuple p
      ; p.parse_fun p
      ; p.parse_function p
      ; p.parse_let_in p
      ; p.parse_match_with p
      ; p.parse_if_then_else p
      ; p.parse_empty_list_expr
      ; p.parse_constant_expr
      ; p.parse_identifier_expr
      ]
  in

  let* decl = 
    skip_wspace *> string "let" *> skip_wspace1 *>
    option "" (string "rec" <* skip_wspace1)
  in

  let* args = many1 parse_pattern in

  let main_pattern = List.hd args in

  let* typ_opt =

    let typ_parser = 
      skip_wspace *> char ':' *> skip_wspace *>
      let* typ = parse_type in
      return @@ Some typ
    in
    
    option None typ_parser
  in

  let* expr = tying *> skip_wspace *> parse_expr in

  let* expr = 
    match args with
    | _ :: md :: tl -> EFun ((md, tl), expr) |> return
    | _ -> return expr
  in

  let* expr =
    match typ_opt with
    | Some typ -> return @@ ETyped (expr, typ)
    | _ -> return expr
  in

  let declaration_type pattern body = 
    match decl with
    | "rec" -> DRecursive (pattern, body)
    | _ -> DOrdinary (pattern, body)
  in

  return (declaration_type main_pattern expr)
;;

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast.AbstractSyntaxTree
open Common

let parse_declaration =
  let* decl =
    skip_wspace *> string "let" *> skip_wspace1 *> option "" (string "rec" <* skip_wspace1)
  in
  let parse_binding () =
    let* args = many1 Pattern.parse_pattern in
    let main_pattern = List.hd args in
    let* typ_opt =
      let typ_parser =
        skip_wspace
        *> char ':'
        *> skip_wspace
        *>
        let* typ = Type.parse_type in
        return @@ Some typ
      in
      option None typ_parser
    in
    let* expr = tying *> skip_wspace *> Expression.parse_expression in
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
    return (main_pattern, expr)
  in
  let* main_binding = parse_binding () in
  let* and_bindings =
    many (skip_wspace *> string "and" *> skip_wspace1 *> parse_binding ())
  in
  let declaration_type main_binding and_bindings =
    match decl with
    | "rec" -> DRecursive (main_binding, and_bindings)
    | _ -> DOrdinary (main_binding, and_bindings)
  in
  return (declaration_type main_binding and_bindings)
;;

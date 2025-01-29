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
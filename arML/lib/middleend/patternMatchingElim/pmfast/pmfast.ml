(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree

type pmf_case = identifier * pmf_expression [@@deriving show { with_path = false }]

and pmf_expression =
  | PMFConstant of constant
  | PMFIdentifier of identifier
  | PMFApplication of pmf_expression * pmf_expression * pmf_expression list
  | PMFLetIn of case * case list * pmf_expression
  | PMFIfThenElse of pmf_expression * pmf_expression * pmf_expression option
  | PMFTuple of pmf_expression * pmf_expression * pmf_expression list
  | PMFListConstructor of pmf_expression * pmf_expression
  | PMFEmptyList
  | PMFTyped of pmf_expression * type_definition
[@@deriving show { with_path = false }]

type pmf_decl_case = identifier * identifier list * pmf_expression [@@deriving show { with_path = false }]

type pmf_decl =
  | PMFDOrdinary of pmf_decl_case * pmf_decl_case list
  | PMFDRecursive of pmf_decl_case * pmf_decl_case list
[@@deriving show { with_path = false }]

type pmf_program = pmf_decl list [@@deriving show { with_path = false }]

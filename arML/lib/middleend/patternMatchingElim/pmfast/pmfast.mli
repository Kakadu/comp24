(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree

(** Binding identifier -> expression *)
type pmf_case = identifier * pmf_expression [@@deriving show { with_path = false }]

and pmf_expression =
  | PMFConstant of constant (** Any constant: '1', 'true', etc *)
  | PMFIdentifier of identifier (** A variable name: 'x', 'y', etc *)
  | PMFApplication of pmf_expression * pmf_expression * pmf_expression list
  (** Application: f x *)
  | PMFLetIn of pmf_case * pmf_expression
  (** Let in (without fun and pattern matching): 'let f = 1 and g = 2 in f + g *)
  | PMFIfThenElse of pmf_expression * pmf_expression * pmf_expression option
  (** if condition then true_branch else false branch (else option) *)
  | PMFTuple of pmf_expression * pmf_expression * pmf_expression list
  (** Tuple: '(E1, E2, ..., En)' *)
  | PMFListConstructor of pmf_expression * pmf_expression
  (** List construction: 1 :: 2 :: [] *)
  | PMFEmptyList (** Empty list: '[]' *)
  | PMFTyped of pmf_expression * type_definition
  (** Assigning a type to an expression: (expr : int) *)
[@@deriving show { with_path = false }]

(** Binding main identifier * arguments identifiers * expression *)
type pmf_decl_case = identifier * identifier list * pmf_expression
[@@deriving show { with_path = false }]

type pmf_decl =
  | PMFDOrdinary of pmf_decl_case (** Top-level let-binding: 'let f x = x *)
  | PMFDRecursive of pmf_decl_case * pmf_decl_case list
  (** Top-level recursive let-binding: 'let rec f x = g x and g x = f x' *)
[@@deriving show { with_path = false }]

(** The entire lambda lifted code of the program *)
type pmf_program = pmf_decl list [@@deriving show { with_path = false }]

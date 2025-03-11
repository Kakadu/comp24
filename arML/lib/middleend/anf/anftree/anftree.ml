(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree

type immut_expr =
  | IConstant of constant
  | IEmptyList
  | IIdentifier of identifier
  | ITuple of immut_expr * immut_expr * immut_expr list
[@@deriving show { with_path = false }]

type complex_expr =
  | CAtom of immut_expr
  | CApplication of immut_expr * immut_expr * immut_expr list
  | CIfThenElse of immut_expr * a_expr * a_expr
  | CListConstructor of immut_expr * immut_expr
  | CTyped of complex_expr * type_definition
[@@deriving show { with_path = false }]

and a_expr =
  | ALetIn of identifier * complex_expr * a_expr
  | AComplex of complex_expr
[@@deriving show { with_path = false }]

type anf_decl_case = identifier * identifier list * a_expr
[@@deriving show { with_path = false }]

type anf_decl =
  | ADOrdinary of anf_decl_case
  | ADRecursive of anf_decl_case * anf_decl_case list
[@@deriving show { with_path = false }]

type anf_program = anf_decl list [@@deriving show { with_path = false }]

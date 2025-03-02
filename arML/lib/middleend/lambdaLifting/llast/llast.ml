(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree

type ll_case = pattern * ll_expr (** Binding pattern -> expression *)

and ll_expr =
  | LEConstant of constant (** Any constant: '1', 'true', etc *)
  | LEIdentifier of identifier (** A variable name: 'x', 'y', etc *)
  | LEApplication of ll_expr * ll_expr * ll_expr list (** Application: f x *)
  | LELetIn of ll_case * ll_case list * ll_expr (** Let in (without fun): 'let f = 1 and g = 2 in f + g *)
  | LEIfThenElse of ll_expr * ll_expr * ll_expr option (** if condition then true_branch else false branch (else option) *)
  | LETuple of ll_expr * ll_expr * ll_expr list (** Tuple: '(E1, E2, ..., En)' *)
  | LEEmptyList (** Empty list: '[]' *)
  | LEListConstructor of ll_expr * ll_expr (** List construction: 1 :: 2 :: [] *)
  | LEMatchWith of ll_expr * ll_case * ll_case list (** Pattern matching: match x with | y -> y *)
  | LETyped of ll_expr * type_definition(** Assigning a type to an expression: (expr : int) *)
[@@deriving show { with_path = false }]

type ll_decl_case = pattern * pattern list * ll_expr (** Binding mainPattern * patterns * expression *)

and ll_decl =
  | LDOrdinary of ll_decl_case * ll_decl_case list (** Top-level let-binding: 'let f x = x and g x = x' *)
  | LDRecursive of ll_decl_case * ll_decl_case list
  (** Top-level recursive let-binding: 'let rec f x = g x and g x = f x' *)
[@@deriving show { with_path = false }]

(** The entire lambda lifted code of the program *)
type ll_program = ll_decl list [@@deriving show { with_path = false }]

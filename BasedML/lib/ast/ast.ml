(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

(* Standard types: ints, functions, tuples, lists *)
type typeName =
  | TInt
  | TPoly of string
  | TTuple of typeName list
  | TFunction of typeName * typeName
  | TList of typeName
[@@deriving show { with_path = false }]

(* Flag to tell implicitely tell if let is recurisve *)
type rec_flag =
  | Rec
  | NotRec
[@@deriving show { with_path = false }]

(* Standard data types (constants): integers, bools*)
type constant =
  | CInt of int
  | CBool of bool
[@@deriving show { with_path = false }]

(* Lists, tuples, identifiers, wild card patterns*)
type pattern =
  | PWildCard
  | PNil
  | PCons of pattern * pattern
  | PIdentifier of string
  | PTuple of pattern list
  | PList of pattern list
  | PConstant of constant
  | PConstraint of pattern * typeName
[@@deriving show { with_path = false }]

(* Standard expressions *)
type expr =
  | EConstant of constant
  | EIdentifier of string
  | EFunction of pattern * expr
  | EApplication of expr * expr
  | EIfThenElse of expr * expr * expr
  | ELetIn of rec_flag * pattern * expr * expr
  | ETuple of expr list
  | EList of expr list
  | EMatch of pattern * (pattern * expr) list
  | EConstraint of expr * typeName
[@@deriving show { with_path = false }]

(* Let binding/declarations *)
type let_declaration =
  | DLet of rec_flag * pattern * expr
  | DMutualRecDecl of let_declaration list
[@@deriving show { with_path = false }]

(* A collections of let declarations*)
type declarations = let_declaration list [@@deriving show { with_path = false }]

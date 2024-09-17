(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

(* Flag to tell implicitely tell if let is recurisve *)
type rec_flag =
  | Rec
  | NotRec
[@@deriving show { with_path = false }]

(* Standard binary operations *)
type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | NotEq
  | Gr
  | GrOrEq
  | Ls
  | LsOrEq
  | Cons
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
[@@deriving show { with_path = false }]

(* Standard expressions *)
type expr =
  | EConstant of constant
  | EBinop of expr * binop * expr
  | EIdentifier of string
  | EFunction of pattern * expr
  | EApplication of expr * expr
  | EIfThenElse of expr * expr * expr
  | ELetIn of rec_flag * pattern * expr * expr
  | ETuple of expr list
  | EList of expr list
  | EMatch of pattern * (pattern * expr) list
[@@deriving show { with_path = false }]

(* Let binding/declarations *)
type let_declaration = DLet of rec_flag * pattern * expr
[@@deriving show { with_path = false }]

(* A collections of let declarations*)
type declarations = let_declaration list [@@deriving show { with_path = false }]

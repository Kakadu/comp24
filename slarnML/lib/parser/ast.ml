(** Copyright 2023-2024, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type const =
  | CInt of int
  | CBool of bool
  | CUnit
[@@deriving show { with_path = false }]

type args = string list [@@deriving show { with_path = false }]

type decl =
  | DeclRec of string * args
  | Decl of string * args
[@@deriving show { with_path = false }]

type expr =
  | Id of string
  | Const of const
  | Not of expr (* not expr *)
  | Or of expr * expr (* expr || expr *)
  | And of expr * expr (* expr && expr *)
  | Eq of expr * expr (* expr = expr *)
  | Gt of expr * expr (* expr > expr *)
  | Lt of expr * expr (* expr < expr *)
  | Gte of expr * expr (* expr >= expr *)
  | Lte of expr * expr (* expr <= expr *)
  | Add of expr * expr (* expr + expr *)
  | Sub of expr * expr (* expr - expr *)
  | Mul of expr * expr (* expr * expr *)
  | Div of expr * expr (* expr / expr *)
  | If of expr * expr * expr (* if expr then expr else expr *)
  | Let of decl * expr (* let id arg1 arg2 = expr *)
  | LetIn of decl * expr * expr (* let id arg1 arg2 = expr in expr *)
  | Fun of args * expr (* fun arg1 arg2 -> expr *)
  | App of expr * expr list
[@@deriving show { with_path = false }]

type ast = expr list [@@deriving show { with_path = false }]

(** Copyright 2024-2025, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ast

type c_expr =
  | CId of string
  | CConst of const
  | CNot of c_expr
  | COr of c_expr * c_expr
  | CAnd of c_expr * c_expr
  | CEq of c_expr * c_expr
  | CGt of c_expr * c_expr
  | CLt of c_expr * c_expr
  | CGte of c_expr * c_expr
  | CLte of c_expr * c_expr
  | CAdd of c_expr * c_expr
  | CSub of c_expr * c_expr
  | CMul of c_expr * c_expr
  | CDiv of c_expr * c_expr
  | CIf of c_expr * c_expr * c_expr
  | CLet of decl * c_expr
  | CLetIn of decl * c_expr * c_expr
  | CFun of args * c_expr (* anon fun *)
  | CApp of c_expr * c_expr list
[@@deriving show { with_path = false }]

type cc_ast = c_expr list [@@deriving show { with_path = false }]

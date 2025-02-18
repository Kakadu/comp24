(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(** Simplified AST *)

type sexpr =
  | SEConst of Ast.constant
  | SEVar of Ast.identifier
  | SETuple of sexpr list
  | SEFun of Ast.identifier list * sexpr
  | SELet of Ast.rec_flag * Ast.identifier * sexpr * sexpr
  | SEApp of sexpr * sexpr
  | SEIf of sexpr * sexpr * sexpr
  | SECons of sexpr * sexpr
[@@deriving eq, show { with_path = false }]

type sstructure_item =
  | SSILet of Ast.rec_flag * (Ast.identifier * sexpr) list
  | SSIExpr of sexpr
[@@deriving eq, show { with_path = false }]

type sstructure = sstructure_item list [@@deriving eq, show { with_path = false }]

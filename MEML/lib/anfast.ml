(** Copyright 2024-2025, Perevalov Efim, Ermolovich Anna *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type aconst =
  | AInt of int
  | ABool of bool
  | ANil
[@@deriving show { with_path = false }]

type aexpression =
  | AVar of name
  | AVars of aexpression * aexpression
  | AConst of aconst
  | AIfElse of aexpression * aexpression * aexpression
  | ABinOp of binary_op * aexpression * aexpression
  | APatLetIn of pattern * aexpression * aexpression
  | ALetIn of name * aexpression * aexpression
  | AApp of aexpression * aexpression
  | ATuple of aexpression list
  | AList of aexpression * aexpression
[@@deriving show { with_path = false }]

type abindings =
  | ALet of (rec_flag * name * name list * aexpression)
  | ALetPat of (pattern * aexpression)
  | AExpression of aexpression
[@@deriving show { with_path = false }]

(** Copyright 2024-2025, Perevalov Efim, Ermolovich Anna *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type llexpression =
  | LLVars of llexpression * llexpression
  | LLConst of const
  | LLVar of name
  | LLIfElse of llexpression * llexpression * llexpression
  | LLEbinOp of binary_op * llexpression * llexpression
  | LLPatLetIn of pattern * llexpression * llexpression
  | LLLetIn of name * llexpression * llexpression
  | LLApp of llexpression * llexpression
  | LLTuple of llexpression list
  | LLMatch of llexpression * (pattern * llexpression) list
  | LLList of llexpression * llexpression
[@@deriving show { with_path = false }]

type llbindings =
  | LLLet of (rec_flag * name * pattern list * llexpression)
  | LLLetPat of (pattern * llexpression)
  | LLExpression of llexpression
[@@deriving show { with_path = false }]

type llstatements = llbindings list [@@deriving show { with_path = false }]

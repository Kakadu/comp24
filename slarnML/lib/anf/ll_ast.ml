(** Copyright 2024-2025, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type l_expr =
    | LId of string
    | LConst of Ast.const
    | LNot of l_expr
    | LOr of l_expr * l_expr
    | LAnd of l_expr * l_expr
    | LEq of l_expr * l_expr
    | LGt of l_expr * l_expr
    | LLt of l_expr * l_expr
    | LGte of l_expr * l_expr
    | LLte of l_expr * l_expr
    | LAdd of l_expr * l_expr
    | LSub of l_expr * l_expr
    | LMul of l_expr * l_expr
    | LDiv of l_expr * l_expr
    | LIf of l_expr * l_expr * l_expr
    | LApp of string * l_expr list
[@@deriving show { with_path = false }]

type gl_expr = 
    | LFun of string * string list * l_expr (* declare function *)

type ll_ast = gl_expr list [@@deriving show { with_path = false }]

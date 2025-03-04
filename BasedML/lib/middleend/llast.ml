(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

(* Pretty much the same as the original AST but without anonymous functions *)
type llexpr =
  | LLConstant of constant
  | LLIdentifier of string
  | LLIfThenElse of llexpr * llexpr * llexpr
  | LLApplication of llexpr * llexpr
  | LLLetIn of rec_flag * pattern * llexpr * llexpr
  | LLConstraint of llexpr * type_name
  | LLTuple of llexpr list
  | LLMatch of llexpr * (pattern * llexpr) list
[@@deriving show { with_path = false }]

type llbinding = LLLet of pattern * pattern list * llexpr
[@@deriving show { with_path = false }]

type lllet_declaration =
  | LLDSingleLet of rec_flag * llbinding
  | LLDMutualRecDecl of rec_flag * llbinding list (**List.length >= 2 *)
[@@deriving show { with_path = false }]

type llprogram = lllet_declaration list [@@deriving show { with_path = false }]

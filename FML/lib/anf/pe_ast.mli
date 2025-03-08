(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type rec_flag =
  | Rec
  | NoRec

type pe_const =
  | Pe_Cint of int
  | Pe_CBool of bool

type pe_expr =
  | Pe_EUnit
  | Pe_ENill
  | Pe_EIdentifier of string
  | Pe_EConst of pe_const
  | Pe_EIf of pe_expr * pe_expr * pe_expr
  | Pe_EFun of string list * pe_expr
  | Pe_EApp of pe_expr * pe_expr
  | Pe_ELet of rec_flag * string * pe_expr * pe_expr
  | Pe_ECons of pe_expr * pe_expr
  | Pe_ETuple of pe_expr list

type pe_declaration =
  | Pe_Nonrec of (string * pe_expr) list
  | Pe_Rec of (string * pe_expr) list

type pe_program = pe_declaration list

val const_to_str : pe_const -> string
val expr_to_str : pe_expr -> string
val decl_to_str : pe_declaration -> string
val pp_pe_structure : Format.formatter -> pe_declaration list -> unit

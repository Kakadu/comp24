(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type rp_const =
  | Rp_c_int of int
  | Rp_c_bool of bool
  | Rp_c_empty_list
  | Rp_c_unit

type rp_expr =
  | Rp_e_const of rp_const
  | Rp_e_ident of string
  | Rp_e_ite of rp_expr * rp_expr * rp_expr
  | Rp_e_fun of string list * rp_expr
  | Rp_e_app of rp_expr * rp_expr
  | Rp_e_let of rp_decl * rp_expr
  | Rp_e_cons_list of rp_expr * rp_expr
  | Rp_e_tuple of rp_expr list

and rp_decl =
  | Rp_non_rec of string * rp_expr
  | Rp_rec of (string * rp_expr) list

type rp_program = rp_decl list

val pp_rp_program : Format.formatter -> rp_program -> unit
val run_remove_patterns_program : Ast.program -> rp_program

(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type rec_flag =
  | Rec (** rec *)
  | NoRec (** norec*)

type pe_const =
  | Pe_Cint of int (** 1 *)
  | Pe_CBool of bool (** true *)

type pe_expr =
  | Pe_EUnit (** () *)
  | Pe_ENill (** [] *)
  | Pe_EIdentifier of string (** x *)
  | Pe_EConst of pe_const (** 1 || true *)
  | Pe_EIf of pe_expr * pe_expr * pe_expr (** if E1 then E2 else E3*)
  | Pe_EFun of string list * pe_expr (** fun x y -> E *)
  | Pe_EApp of pe_expr * pe_expr (** E1 E2 *)
  | Pe_ELet of rec_flag * string * pe_expr * pe_expr (** let (rec) f = E1 in E2 *)
  | Pe_ECons of pe_expr * pe_expr (** E1 :: E2 *)
  | Pe_ETuple of pe_expr list (** (E1, E2, E3) *)

type pe_declaration =
  | Pe_Nonrec of (string * pe_expr) list (** (let f1 = E1 (and f2 = E2) *)
  | Pe_Rec of (string * pe_expr) list (** (let rec f1 = E1 (and f2 = E2) *)

type pe_program = pe_declaration list

val const_to_str : pe_const -> string
val expr_to_str : pe_expr -> string
val decl_to_str : pe_declaration -> string
val pp_pe_structure : Format.formatter -> pe_declaration list -> unit

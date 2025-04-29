(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type rec_flag =
  | Rec (** let rec ... *)
  | NoRec (** let ... *)

type me_const =
  | Me_Cint of int (** 52 *)
  | Me_CBool of bool (** true *)

type me_expr =
  | Me_EUnit (** () *)
  | Me_ENill (** [] *)
  | Me_EIdentifier of string (** f *)
  | Me_EConst of me_const (** 52 || true *)
  | Me_EIf of me_expr * me_expr * me_expr (** if E1 then E2 else E3 *)
  | Me_EFun of string list * me_expr (** fun a,b,c -> E *)
  | Me_EApp of me_expr * me_expr (** E1 E2*)
  | Me_ELet of rec_flag * string * me_expr * me_expr (** let [rec] name = E1 in E2 *)
  | Me_ECons of me_expr * me_expr (** E1::E2 *)
  | Me_ETuple of me_expr list (** (E1, E2, E3) *)

type me_declaration =
  | Me_Nonrec of (string * me_expr) list (** let f = E *)
  | Me_Rec of (string * me_expr) list (** let rec f = E *)

(** me_declaration;; me_declaration *)
type me_program = me_declaration list

val pp_me_program : Format.formatter -> me_declaration list -> unit

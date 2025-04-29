(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type immexpr =
  | ImmInt of int (** 52 *)
  | ImmIdentifier of string (** f *)
  | ImmBool of bool (** true *)
  | ImmUnit (** () *)
  | ImmNill (** [] *)
  | ImmTuple of immexpr list (** (IE, IE, IE)*)

type cexpr =
  | CEApply of string * immexpr list (** f IE1 IE2 IE3 ... *)
  | CEIf of immexpr * aexpr * aexpr (** if IE then AE1 else AE2 *)
  | CECons of immexpr * immexpr (** IE::IE*)
  | CImmExpr of immexpr (** IE *)

and aexpr =
  | ALetIn of string * cexpr * aexpr (** let a = CE in AE*)
  | ACExpr of cexpr (** CE *)

type anf_binding = ALet of string * string list * aexpr (** f a b c = AE*)

type anf_decl =
  | Based_value of string * aexpr (** let f = AE*)
  | ADNoRec of anf_binding list (** let rec anf_binding *)
  | ADRec of anf_binding list (** let anf_binding *)

(** anf_decl;; anf_decl *)
type anf_prog = anf_decl list

val imm_id : string -> immexpr
val cimmexpr : immexpr -> cexpr
val pp_anf_program : Format.formatter -> anf_decl list -> unit

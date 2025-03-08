(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type immexpr =
  | ImmInt of int
  | ImmIdentifier of string
  | ImmBool of bool
  | ImmUnit
  | ImmNill
  | ImmTuple of immexpr list

type cexpr =
  | CEApply of cexpr * cexpr
  | CEIf of immexpr * aexpr * aexpr
  | CECons of immexpr * immexpr
  | CImmExpr of immexpr

and aexpr =
  | ALetIn of string * cexpr * aexpr
  | ACExpr of cexpr

type anf_binding = ALet of string * string list * aexpr

type anf_decl =
  | ADNoRec of anf_binding
  | ADREC of anf_binding list

type anf_prog = anf_decl list

let imm_id id = ImmIdentifier id
let cimmexpr immexpr = CImmExpr immexpr

(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

type immexpr =
  | ImmInt of int
  | ImmIdentifier of string
  | ImmBool of bool
  | ImmUnit
  | ImmNil
  | ImmTuple of immexpr list
  | ImmConstraint of immexpr * type_name

type cexpr =
  | CApplication of immexpr * immexpr * immexpr list
  | CIfThenElse of immexpr * aexpr * aexpr
  | CImmExpr of immexpr

and aexpr =
  | ALetIn of string * cexpr * aexpr
  | ACExpr of cexpr

type single_anf_binding = ALet of string * string list * aexpr

type anf_decl =
  | ADSingleLet of rec_flag * single_anf_binding
  | ADMutualRecDecl of single_anf_binding list (**List.length >= 2 *)

type anf_prog = anf_decl list

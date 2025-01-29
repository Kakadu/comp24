(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

type immexpr =
  | ImmInt of int
  | ImmIdentifier of string
  | ImmBool of bool
  | ImmUnit
  | ImmNil

type cexpr =
  | CApplication of immexpr * immexpr
  | CIfThenElse of immexpr * aexpr * aexpr
  | CTuple of immexpr list
  | CMatch of pattern * (pattern * aexpr) list
  | CImmExpr of immexpr
  | CConstraint of immexpr * type_name

and aexpr =
  | ALetIn of pattern * cexpr * aexpr
  | ACExpr of cexpr

type single_anf_binding = ALet of pattern * pattern list * aexpr

type anf_decl =
  | ADSingleLet of rec_flag * single_anf_binding
  | ADMutualRecDecl of rec_flag * single_anf_binding list (**List.length >= 2 *)

type anf_prog = anf_decl list

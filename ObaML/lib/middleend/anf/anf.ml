(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type immexpr =
  | ImmId of string
  | ImmInt of int
  | ImmString of string
  | ImmBool of bool
  | ImmEmptyList
  | ImmUnit
  | ImmTuple of immexpr list

type cexpr =
  | CImmExpr of immexpr
  | CApp of string * immexpr list
  | CIf of immexpr * aexpr * aexpr
  | CCons of immexpr * immexpr

and aexpr =
  | ALet of string * cexpr * aexpr
  | ACExpr of cexpr

type tp = bool * (string * string list * aexpr) list
type program = tp list

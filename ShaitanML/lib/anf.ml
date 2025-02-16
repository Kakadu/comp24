(** Copyright 2024-2025, Nikita Lukonenko and Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type immexpr =
  | ImmNum of int
  | ImmId of string

type cexpr =
  | CApply of immexpr * immexpr
  | CImmExpr of immexpr

type aexpr =
  | ALet of rec_flag * (pattern * cexpr) list * aexpr
  | ACExpr of cexpr

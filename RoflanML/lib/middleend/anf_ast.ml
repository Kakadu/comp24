(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Ast

type immexpr =
  | ImmConst of const
  | ImmVar of id
  | ImmTuple of immexpr * immexpr * immexpr list
  | ImmList of immexpr list

type cexpr =
  | CImm of immexpr
  | CBranch of immexpr * immexpr * immexpr
  | CMatch of immexpr * (pattern * aexpr) list
  | CApp of immexpr * immexpr

and aexpr =
  | ALet of id * cexpr * aexpr
  | ACExpr of cexpr

type adecl = ADLet of is_rec * id * typed_arg list * aexpr
type aprogram = adecl list

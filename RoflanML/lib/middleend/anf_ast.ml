(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type immexpr =
  | ImmConst of const
  | ImmVar of id
  | ImmTuple of immexpr * immexpr * immexpr list
  | ImmList of immexpr list

type cexpr =
  | CImm of immexpr
  | CBranch of cexpr * cexpr * cexpr
  | CMatch of immexpr

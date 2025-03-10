(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Parser.Ast

type aexpr =
  | AExp_ident of string
  | AExp_constant of constant
  | AExp_tuple of aexpr list
  | AExp_construct of string * aexpr option

type cexpr =
  | CExp_if of aexpr * lexpr * lexpr
  | CExp_apply of string * aexpr list
  | CExp_atom of aexpr

and lexpr =
  | LLet_in of string * cexpr * lexpr
  | LComplex of cexpr

type func = string * string list * lexpr

type absexpr =
  | AbsStr_func of func
  | AbsStr_value of string * lexpr
  | AbsStr_value_rec of func list

type anf = absexpr list

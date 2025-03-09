(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Parser.Ast

type aexpr =
  | AExp_ident of string
  | AExp_constant of constant
  | AExp_tuple of aexpr list
  | AExp_construct of string * aexpr option

type cexpr =
  | CExp_if of aexpr * cexpr * cexpr
  | CExp_apply of aexpr * aexpr list
  | CExp_atom of aexpr

and lexp =
  | LLet_int of string * cexpr * lexp
  | LComplex of cexpr

type func = string * string list * cexpr

type absexpr =
  | AbsStr_eval of cexpr
  | AbsStr_func of func
  | AbsStr_value of string * cexpr
  | AbsStr_value_rec of func list

type anf = absexpr list

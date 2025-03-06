(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Parser.Ast

type rexpr =
  | RExp_ident of string (** like Exp_ident *)
  | RExp_constant of constant (** like Exp_constant *)
  | RExp_let of string * rexpr * rexpr (** e.g. [let a = b in c] *)
  | RExp_let_rec of (string * rexpr) list * rexpr
  (** e.g. [let rec a = b and c = a in d]*)
  | RExp_fun of string list * rexpr (** like Exp_fun but only non pattern args *)
  | RExp_apply of rexpr * rexpr (** like Exp_apply *)
  | RExp_tuple of rexpr list (**  like Exp_tuple *)
  | RExp_construct of string * rexpr option (** like Exp_construct *)
  | RExp_if of rexpr * rexpr * rexpr option (** like Exp_if *)

type rstuct_item =
  | RStr_eval of rexpr
  | RStr_value of string * rexpr
  | RStr_value_rec of (string * rexpr) list

type rstruct = rstuct_item list

type error =
  | Invalid_pattern of string
  | Invalid_ast of string

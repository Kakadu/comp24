(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Base
open Angstrom
open Ast
open Utils

let parse_ident =
  let+ ident = parse_lowercase_ident in
  Exp_ident ident
;;

let parse_const =
  let+ const = Const_parser.parse_const in
  Exp_constant const
;;

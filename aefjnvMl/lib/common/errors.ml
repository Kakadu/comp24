(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree

type parse_error = Syntax_error of string
type illegal_state_error = Illegal_state_error of string

type infer_error =
  | Occurs_check of int * ty
  | Unbound_variable of string
  | Unification_failed of ty * ty
  | Several_bounds of string
  | Not_specify_rec
  | No_variable_rec

type error =
  | Parser of parse_error
  | Infer of infer_error
  | Middleend of illegal_state_error

let occurs_check (b, t) = Infer (Occurs_check (b, t))
let unbound_variable v = Infer (Unbound_variable v)
let unification_failed (t1, t2) = Infer (Unification_failed (t1, t2))
let several_bounds v = Infer (Several_bounds v)
let not_specify_rec = Infer Not_specify_rec
let no_variable_rec = Infer No_variable_rec
let illegal_state msg = Middleend (Illegal_state_error msg)

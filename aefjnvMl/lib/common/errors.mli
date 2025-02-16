(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree

type parse_error = Syntax_error of string

type infer_error =
  | Occurs_check of int * ty
  (** This error can lead to non-terminate of the type inference
      Example: [fun x -> x x]*)
  | Unbound_variable of string
  (** Variable is not defined in local context
      Example: [let increase = x + 1]*)
  | Unification_failed of ty * ty (** Unification error between two types *)
  | Several_bounds of string (** Example: [let (x, x) = 1, 3] *)
  | Not_specify_rec
  | No_variable_rec
  (** Only variables are allowed as left-side of 'let rec'
      Example: [let rec (a, b) = 5] *)

type illegal_state_error = Illegal_state_error of string

type error =
  | Parser of parse_error
  | Infer of infer_error
  | Alpha_converter of illegal_state_error

(*================Constructors================*)

val occurs_check : binder * ty -> error
val unbound_variable : string -> error
val unification_failed : ty * ty -> error
val several_bounds : string -> error
val not_specify_rec : error
val no_variable_rec : error
val illegal_state : string -> error

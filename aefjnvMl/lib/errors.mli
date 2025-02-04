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

(* type interpreter_error =
  | Division_by_zero (** Example: [5 / 0] *)
  | Match_failure
  (** May occurs with non-exhaustive pattern-matching, if value doesn't match any case *)
  | Invalid_compare_arg of string
  (** Occurs when trying to compare two anonymous functions *)
  | Ill_right_side_rec of string (** Represents limitations of `let rec` *)
  | Ill_typed (* Unreachable with using infer *)
  | Unbound_var of string Unreachable with using infer *)

type error =
  | Parser of parse_error
  | Infer of infer_error
(* | Interpreter of interpreter_error *)

(*================Constructors================*)

val occurs_check : binder * ty -> error
val unbound_variable : string -> error
val unification_failed : ty * ty -> error
val several_bounds : string -> error
val not_specify_rec : error
val no_variable_rec : error

(*
   val multiple_variable : string -> error
val multiple_method : string -> error
val undefined_method : ty -> string -> error
val not_object : ty -> error
val cannot_match_self : error
val division_by_zero : error
val match_failure : error
val invalid_compare_arg : string -> error
val ill_right_side_rec : string -> error
val ill_typed : error
val unbound_var : string -> error *)

[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Angstrom

open LMisc
open LAst
open LTypes

(* ======= Utils ======= *)
val unit : unit t
val ws : unit t
val ws1 : unit t
val ident : string -> Id.t t

val parens : 'a t -> 'a t
val spaced : 'a t -> 'a t
val opt : 'a t -> 'a option t

(* ======= Identifiers ======= *)
val pconstruct_id : Id.t t
val pvalue_id : Id.t t
val pty_con_id : Id.t t
val pty_var : Var.t t

val pinfix_id : ?starts:string -> unit -> Id.t t
val pprefix_id : Id.t t

(* ===== Constants, value bindings ===== *)
val pconst : Const.t t
val plet :
     Expr.t t
  -> Pat.t t
  -> Ty.t t
  -> (Expr.rec_flag * Expr.value_binding List1.t) t

(* ======= Operators ======= *)
type 'oprnd operator =
  | Prefix of ('oprnd -> 'oprnd) t  (** Prefix operator *)
  | InfixN of ('oprnd List2.t -> 'oprnd) t
      (** Non-associative infix operator. Parsed to a list of operands *)
  | InfixL of ('oprnd -> 'oprnd -> 'oprnd) t
      (** Left associative infix operator *)
  | InfixR of ('oprnd -> 'oprnd -> 'oprnd) t
      (** Right associative infix operator *)

val poperators : table:'oprnd operator list list -> poprnd:'oprnd t -> 'oprnd t
(**
  Table is ordered in descending precedence.
  All operators in one list have the same precedence
  (but may have different associativity)
 *)

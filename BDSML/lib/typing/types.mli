(** Copyright 2024-2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module TVarId : sig
  type t

  val to_string : t -> string
  val create : int -> t
  val compare : t -> t -> int
  val ( + ) : t -> int -> t
end

type base_type =
  | TInt
  | TChar
  | TString
  | TBool

type type_val =
  | TVar of TVarId.t (** e.g. ['a] *)
  | TBase of base_type (** e.g. [int] *)
  | TConstructor of type_val option * string (** e.g. [int list] *)
  | TTuple of type_val list (** e.g. [int * int] *)
  | TArrow of type_val * type_val (** e.g. [int -> int] *)
  | TUnit (** e.g. [unit]*)

val pp_type_val : Format.formatter -> type_val -> unit
val show_type_val : type_val -> string

type error =
  | Unification_failed of type_val * type_val
  | Occurs_check
  | No_variable of string
  | Invalid_let
  | Invalid_list_constructor_argument
  | Invalid_ast of string
  | Invalid_predefined_operators of string

exception Unimplemented of string

module VarSet : Set.S with type elt = TVarId.t

val occurs_in : TVarId.t -> type_val -> bool
val free_vars : type_val -> VarSet.t

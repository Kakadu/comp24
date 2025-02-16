(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module VarId = struct
  type t = int [@@deriving show { with_path = false }]

  let to_string n = Int.to_string n
  let create n : t = n
  let compare = Int.compare
  let ( + ) = ( + )
end

type base_type =
  | TInt
  | TChar
  | TString
  | TBool
[@@deriving show { with_path = false }]

type type_val =
  | TVar of VarId.t (** e.g. ['a] *)
  | TBase of base_type (** e.g. [int] *)
  | TParametric of type_val * type_val (** e.g. [int list] *)
  | TTuple of type_val list (** e.g. [int * int] *)
  | TArrow of type_val * type_val (** e.g. [int -> int] *)
[@@deriving show { with_path = false }]

type error =
  | Unification_failed of type_val * type_val
  | Occurs_check

exception Unimplemented of string

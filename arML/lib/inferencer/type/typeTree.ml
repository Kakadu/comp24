(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type type_var = int

let pp_type_var = Format.pp_print_int

module TypeVar = struct
  type t = type_var

  let compare = Int.compare
end

type ground_type =
  | GTInt (** Integer type (int) *)
  | GTBool (** Boolean type (bool) *)
  | GTUnit (** Unit type (unit) *)
  | GTChar (** Character type (char) *)
  | GTString (** String type (string) *)
[@@deriving show { with_path = false }]

type typ =
  | TGround of ground_type (** Basic types: int, bool, unit, etc. *)
  | TVar of type_var (** Polymorphic type variable: 'a, 'b, etc. *)
  | TArr of typ * typ (** Function type: 'a -> 'b *)
  | TTuple of typ list (** Tuple type: 'a * int * char *)
  | TList of typ (** List type: 'a list *)
[@@deriving show { with_path = false }]

module TypeVarSet = Stdlib.Set.Make (TypeVar) (* Set, that storing type variables. *)
module VarSet = Stdlib.Set.Make (String)
(* Set, that storing variables (let-binds and effect declarations). *)

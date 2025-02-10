(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Type variable representation *)
type type_var = int

(** Module for handling type variables *)
module TypeVar : sig
  type t = type_var
  val compare : t -> t -> int
end

(** Primitive (ground) types *)
type ground_type =
  | GTInt    (** Integer type (int) *)
  | GTBool   (** Boolean type (bool) *)
  | GTUnit   (** Unit type (unit) *)
  | GTChar   (** Character type (char) *)
  | GTString (** String type (string) *)
[@@deriving show { with_path = false }]

(** Full type representation *)
type typ =
  | TGround of ground_type  (** Basic types: int, bool, unit, etc. *)
  | TVar of type_var        (** Polymorphic type variable: 'a, 'b, etc. *)
  | TArr of typ * typ       (** Function type: 'a -> 'b *)
  | TTuple of typ list      (** Tuple type: 'a * int * char *)
  | TList of typ            (** List type: 'a list *)
[@@deriving show { with_path = false }]

(** Set, that storing type variables. *)
module TypeVarSet : sig
  include module type of Set.Make (TypeVar)
end

(** Set, that storing variables (let-binds and effect declarations). *)
module VarSet : sig
  include module type of Set.Make (String)
end

(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type type_var = int

type typ =
  | TBool
  | TInt
  | TUnit
  | TVar of type_var
  | TFunction of typ * typ
  | TTuple of typ list
  | TList of typ

module TVarSet : sig
  include module type of Set.Make (Int)
end

module VarSet : sig
  include module type of Set.Make (String)
end

type scheme = Scheme of TVarSet.t * typ

val tint : typ
val tbool : typ
val tunit : typ
val tvar : type_var -> typ
val tfunction : typ -> typ -> typ
val ttuple : typ list -> typ
val tlist : typ -> typ

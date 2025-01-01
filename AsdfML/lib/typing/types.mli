(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type var_id = int

type ground =
  | TInt
  | TBool
  | TUnit

val equal_ground : ground -> ground -> bool

type ty =
  | TGround of ground
  | TVar of var_id
  | TArrow of ty * ty
  | TTuple of ty list
  | TList of ty

type error =
  [ `Arg_num_mismatch of Ast.pattern * ty
  | `No_variable of string
  | `Occurs_check
  | `Syntax_error of string
  | `TODO of string
  | `Unification_failed of ty * ty
  ]

val int_typ : ty
val bool_typ : ty
val unit_typ : ty
val arrow : ty -> ty -> ty
val var : var_id -> ty
val ( ^-> ) : ty -> ty -> ty

type var_id_set = (var_id, Base.Int.comparator_witness) Base.Set.t
type scheme = var_id_set * ty

val an_ty_to_ty : Ast.type_ann -> ty

val dummy_ty : ty

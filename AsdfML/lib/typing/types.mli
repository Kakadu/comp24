(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type var_id = int

val pp_var_id : Format.formatter -> var_id -> unit
val show_var_id : var_id -> string
val equal_var_id : var_id -> var_id -> bool

type ground =
  | TInt
  | TBool
  | TUnit

val pp_ground : Format.formatter -> ground -> unit
val show_ground : ground -> string
val equal_ground : ground -> ground -> bool

type ty =
  | TGround of ground
  | TVar of var_id
  | TArrow of ty * ty
  | TTuple of ty * ty * ty list
  | TList of ty

val pp_ty : Format.formatter -> ty -> unit
val show_ty : ty -> string
val equal_ty : ty -> ty -> bool

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
val count_arrow_args : ty -> int
val dummy_ty : ty
val arg1 : ty
val arg2 : ty
val arg3 : ty

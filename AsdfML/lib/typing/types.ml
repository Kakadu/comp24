(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

(* Based on https://gitlab.com/Kakadu/fp2020course-materials/-/blob/master/code/miniml*)

open Base

type var_id = int [@@deriving show { with_path = false }]

type ground =
  | TInt
  | TBool
  | TUnit
[@@deriving show { with_path = false }, eq]

type ty =
  | TGround of ground
  | TVar of var_id
  | TArrow of ty * ty
  | TTuple of ty list
  | TList of ty
[@@deriving show { with_path = false }]

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of ty * ty
  | `Arg_num_mismatch of Ast.pattern * ty
  | `Syntax_error of string
  | `TODO of string
  ]

let int_typ = TGround TInt
let bool_typ = TGround TBool
let unit_typ = TGround TUnit
let arrow l r = TArrow (l, r)
let var x = TVar x
let ( ^-> ) = arrow

type var_id_set = (var_id, Int.comparator_witness) Set.t
type scheme = var_id_set * ty

let rec an_ty_to_ty = function
  | Ast.TAInt -> int_typ
  | Ast.TABool -> bool_typ
  | Ast.TAUnit -> unit_typ
  | Ast.TAFun (l, r) -> TArrow (an_ty_to_ty l, an_ty_to_ty r)
  | Ast.TATuple (hd1, hd2, tl) ->
    let xs = hd1 :: hd2 :: tl in
    TTuple (List.map xs an_ty_to_ty)
  | Ast.TAList x -> TList (an_ty_to_ty x)
;;

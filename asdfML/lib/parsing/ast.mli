(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type id = string

type rec_flag =
  | Rec
  | NonRec

type constant =
  | CInt of int
  | CBool of bool
  | CUnit
  | CNil

type type_ann =
  | TAInt
  | TABool
  | TAUnit
  | TATuple of type_ann list
  | TAFun of type_ann * type_ann
  | TAList of type_ann

type pattern =
  | PConst of constant
  | PWild
  | PIdent of id
  | PTuple of pattern list
  | PList of pattern list
  | PCons of pattern * pattern
  | PAnn of pattern * type_ann

type expr =
  | EConst of constant
  | EVar of id
  | EApp of expr * expr
  | EIfElse of expr * expr * expr
  | EFun of pattern list * expr
  | ELetIn of definition * expr
  | ETuple of expr list
  | EList of expr list
  | EMatch of expr * (pattern * expr) list

and definition = DLet of rec_flag * pattern * expr
and program = definition list

val p_const : constant -> pattern
val p_wild : pattern
val p_ident : id -> pattern
val p_tuple : pattern list -> pattern
val p_list : pattern list -> pattern
val p_cons : pattern -> pattern -> pattern
val p_ann : pattern -> type_ann -> pattern
val e_const : constant -> expr
val e_var : id -> expr
val e_app : expr -> expr -> expr
val e_if_else : expr -> expr -> expr -> expr
val e_fun : pattern list -> expr -> expr
val e_let_in : definition -> expr -> expr
val e_tuple : expr list -> expr
val e_list : expr list -> expr
val e_match : expr -> (pattern * expr) list -> expr

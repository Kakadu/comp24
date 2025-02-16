(** Copyright 2024-2025, Nikita Lukonenko and Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string

val pp_id : Format.formatter -> id -> unit
val show_id : id -> string

type rec_flag =
  | Rec
  | Nonrec

val pp_rec_flag : Format.formatter -> rec_flag -> unit
val show_rec_flag : rec_flag -> string

type const =
  | CInt of int
  | CBool of bool
  | CString of id
  | CUnit
  | CNil

val pp_const : Format.formatter -> const -> unit
val show_const : const -> string

type type_annot =
  | AInt
  | ABool
  | AString
  | AUnit
  | AList of type_annot
  | AFun of type_annot * type_annot
  | ATuple of type_annot list
  | AVar of id

val pp_type_annot : Format.formatter -> type_annot -> unit
val show_type_annot : type_annot -> string

type pattern =
  | PAny
  | PConst of const
  | PVar of id
  | PTuple of pattern list
  | PCons of pattern * pattern
  | PConstraint of pattern * type_annot

val pp_pattern : Format.formatter -> pattern -> unit
val show_pattern : pattern -> string

type expr =
  | EConst of const
  | EVar of id
  | EIf of expr * expr * expr
  | EMatch of expr * case list
  | ELet of rec_flag * binding * expr
  | EFun of pattern * expr
  | ETuple of expr list
  | ECons of expr * expr
  | EApply of expr * expr
  | EConstraint of expr * type_annot

and case = pattern * expr
and binding = pattern * expr

type str_item =
  | SEval of expr
  | SValue of rec_flag * binding list

type structure = str_item list

val pp_expr : Format.formatter -> expr -> unit
val show_expr : expr -> string
val pp_case : Format.formatter -> case -> unit
val show_case : case -> string
val pp_binding : Format.formatter -> binding -> unit
val show_binding : binding -> string
val pp_str_item : Format.formatter -> str_item -> unit
val show_str_item : str_item -> string
val constr_apply : expr -> expr list -> expr
val constr_fun : pattern list -> expr -> expr
val pp_structure : Format.formatter -> structure -> unit
val show_structure : structure -> string
val print_structure : structure -> string

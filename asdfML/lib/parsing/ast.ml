(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type id = string [@@deriving eq, show { with_path = false }]

type rec_flag =
  | Rec
  | NonRec
[@@deriving show { with_path = false }]

type constant =
  | CInt of int (** 42 *)
  | CBool of bool (** true | false *)
  | CUnit (** () *)
  | CNil (** [] *)
[@@deriving show { with_path = false }]

type type_ann =
  | TAInt (** int *)
  | TABool (** bool *)
  | TAUnit (** () *)
  | TATuple of type_ann list
  | TAFun of type_ann * type_ann (** int -> bool *)
  (* TODO: lists *)
[@@deriving show { with_path = false }]

type pattern =
  | PConst of constant
  | PWild (** _ *)
  | PIdent of id (** x *)
  | PTuple of pattern list
  | PList of pattern list (** [1, 2, 3] *)
  | PCons of pattern * pattern (** hd :: tl *)
  | PAnn of pattern * type_ann (** (x: int) *)
[@@deriving show { with_path = false }]

type expr =
  | EConst of constant (** 42, true, ()*)
  | EVar of id (** x *)
  | EApp of expr * expr (** f x *)
  | EIfElse of expr * expr * expr (** if x then y else z *)
  | EFun of pattern * expr (** fun x -> y *)
  | ELetIn of definition * expr (** let x = y in z *)
  | ETuple of expr list (** (x, fun x -> x, 42) *)
  | EList of expr list (** [1; 2; 3] *)
  | EMatch of expr * (pattern * expr) list (** match x with ... *)
[@@deriving show { with_path = false }]

and definition = DLet of rec_flag * pattern * expr (** let (rec)? x = y *)
[@@deriving show { with_path = false }]

and program = definition list [@@deriving show { with_path = false }]

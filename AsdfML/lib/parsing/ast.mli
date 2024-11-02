(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type id = string

type rec_flag =
  | Rec (** recursive *)
  | NonRec (** non-recursive *)

type constant =
  | CInt of int (** 42 *)
  | CBool of bool (** true | false *)
  | CUnit (** () *)
  | CNil (** [] *)

type type_ann =
  | TAInt (** int *)
  | TABool (** bool *)
  | TAUnit (** () *)
  | TATuple of type_ann list (** int * bool *)
  | TAFun of type_ann * type_ann (** int -> bool *)
  | TAList of type_ann (** %type% list *)

type pattern =
  | PConst of constant
  | PWild (** _ *)
  | PIdent of id (** x *)
  | PTuple of pattern list (** (a, b) *)
  | PList of pattern list (** [1, 2, 3] *)
  | PCons of pattern * pattern (** hd :: tl *)
  | PAnn of pattern * type_ann (** (x: int) *)

type expr =
  | EConst of constant (** 42, true, () *)
  | EVar of id (** x *)
  | EApp of expr * expr (** f x *)
  | EIfElse of expr * expr * expr (** if x then y else z *)
  | EFun of pattern list * expr (** fun x -> y *)
  | ELetIn of definition * expr (** let x = y in z *)
  | ETuple of expr list (** (x, fun x -> x, 42) *)
  | EList of expr list (** [1; 2; 3] *)
  | EMatch of expr * (pattern * expr) list (** match x with ... *)

and definition = DLet of rec_flag * pattern * expr (** let (rec)? x = y *)

type program = definition list

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

(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

val div : int
val gen_id : string QCheck.Gen.t

type id = string

val pp_id : Format.formatter -> id -> unit
val show_id : id -> string
val gen_id : string QCheck.Gen.t
val arb_id : string QCheck.arbitrary
val equal_id : id -> id -> bool

type rec_flag =
  | Rec (** recursive *)
  | NonRec (** non-recursive *)

val pp_rec_flag : Format.formatter -> rec_flag -> unit
val show_rec_flag : rec_flag -> string
val gen_rec_flag : rec_flag QCheck.Gen.t
val arb_rec_flag : rec_flag QCheck.arbitrary
val equal_rec_flag : rec_flag -> rec_flag -> bool

type constant =
  | CInt of int (** 42 *)
  | CBool of bool (** true | false *)
  | CUnit (** () *)
  | CNil (** [] *)

val pp_constant : Format.formatter -> constant -> unit
val show_constant : constant -> string
val gen_constant : constant QCheck.Gen.t
val arb_constant : constant QCheck.arbitrary
val equal_constant : constant -> constant -> bool

type type_ann =
  | TAInt (** int *)
  | TABool (** bool *)
  | TAUnit (** () *)
  | TATuple of type_ann * type_ann * type_ann list (** (int * bool) *)
  | TAFun of type_ann * type_ann (** int -> bool *)
  | TAList of type_ann (** %type% list *)

val pp_type_ann : Format.formatter -> type_ann -> unit
val show_type_ann : type_ann -> string
val gen_type_ann_sized : int -> type_ann QCheck.Gen.t
val gen_type_ann : type_ann QCheck.Gen.t
val arb_type_ann_sized : int -> type_ann QCheck.arbitrary
val arb_type_ann : type_ann QCheck.arbitrary
val equal_type_ann : type_ann -> type_ann -> bool

type pattern =
  | PConst of constant (** 42 *)
  | PWild (** _ *)
  | PIdent of id (** x *)
  | PTuple of pattern * pattern * pattern list (** (a, b) *)
  | PList of pattern list (**  *)
  | PCons of pattern * pattern (** hd :: tl *)
  | PAnn of pattern * type_ann (** (x: int) *)

val pp_pattern : Format.formatter -> pattern -> unit
val show_pattern : pattern -> string
val gen_pattern_sized : int -> pattern QCheck.Gen.t
val gen_pattern : pattern QCheck.Gen.t
val arb_pattern_sized : int -> pattern QCheck.arbitrary
val arb_pattern : pattern QCheck.arbitrary
val equal_pattern : pattern -> pattern -> bool

type expr =
  | EConst of constant (** 42, true, ()*)
  | EVar of id (** x *)
  | EApp of expr * expr (** f x *)
  | EIfElse of expr * expr * expr (** if x then y else z *)
  | EFun of pattern * pattern list * expr (** fun x -> y *)
  | ELetIn of definition * expr (** let x = y in z *)
  | ETuple of expr * expr * expr list (** (x, fun x -> x, 42) *)
  | EList of expr list (**  *)
  | EMatch of (expr * (pattern * expr) list) (** match x with ... *)

and definition = DLet of rec_flag * pattern * expr (** let (rec)? x = y *)

val pp_expr : Format.formatter -> expr -> unit
val show_expr : expr -> string
val pp_definition : Format.formatter -> definition -> unit
val show_definition : definition -> string
val gen_expr_sized : int -> expr QCheck.Gen.t
val gen_definition_sized : int -> definition QCheck.Gen.t
val gen_expr : expr QCheck.Gen.t
val gen_definition : definition QCheck.Gen.t
val arb_expr_sized : int -> expr QCheck.arbitrary
val arb_definition_sized : int -> definition QCheck.arbitrary
val arb_expr : expr QCheck.arbitrary
val arb_definition : definition QCheck.arbitrary
val equal_expr : expr -> expr -> bool
val equal_definition : definition -> definition -> bool

type program = definition list

val pp_program : Format.formatter -> program -> unit
val show_program : program -> string
val gen_program : definition list QCheck.Gen.t
val arb_program : definition list QCheck.arbitrary
val equal_program : program -> program -> bool
val p_const : constant -> pattern
val p_wild : pattern
val p_ident : id -> pattern
val p_tuple : pattern -> pattern -> pattern list -> pattern
val p_list : pattern list -> pattern
val p_cons : pattern -> pattern -> pattern
val p_ann : pattern -> type_ann -> pattern
val e_const : constant -> expr
val e_var : id -> expr
val e_app : expr -> expr -> expr
val e_if_else : expr -> expr -> expr -> expr
val e_fun : pattern -> pattern list -> expr -> expr
val e_let_in : definition -> expr -> expr
val e_tuple : expr -> expr -> expr list -> expr
val e_list : expr list -> expr
val e_match : expr -> (pattern * expr) list -> expr
val d_let : pattern -> expr -> definition
val d_let_rec : pattern -> expr -> definition
val d_let_flag : rec_flag -> pattern -> expr -> definition

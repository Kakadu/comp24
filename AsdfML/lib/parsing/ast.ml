(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type id = string [@@deriving eq, show { with_path = false }]

type rec_flag =
  | Rec (** recursive *)
  | NonRec (** non-recursive *)
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
  | TATuple of type_ann list (** int * bool *)
  | TAFun of type_ann * type_ann (** int -> bool *)
  | TAList of type_ann (** %type% list *)
[@@deriving show { with_path = false }]

type pattern =
  | PConst of constant
  | PWild (** _ *)
  | PIdent of id (** x *)
  | PTuple of pattern list (** (a, b) *)
  | PList of pattern list (** [1, 2, 3] *)
  | PCons of pattern * pattern (** hd :: tl *)
  | PAnn of pattern * type_ann (** (x: int) *)
[@@deriving show { with_path = false }]

type expr =
  | EConst of constant (** 42, true, ()*)
  | EVar of id (** x *)
  | EApp of expr * expr (** f x *)
  | EIfElse of expr * expr * expr (** if x then y else z *)
  | EFun of pattern list * expr (** fun x -> y *)
  | ELetIn of definition * expr (** let x = y in z *)
  | ETuple of expr list (** (x, fun x -> x, 42) *)
  | EList of expr list (** [1; 2; 3] *)
  | EMatch of expr * (pattern * expr) list (** match x with ... *)
[@@deriving show { with_path = false }]

and definition = DLet of rec_flag * pattern * expr (** let (rec)? x = y *)
[@@deriving show { with_path = false }]

type program = definition list [@@deriving show { with_path = false }]

let p_const c = PConst c
let p_wild = PWild
let p_ident x = PIdent x
let p_tuple ps = PTuple ps
let p_list ps = PList ps
let p_cons hd tl = PCons (hd, tl)
let p_ann p t = PAnn (p, t)
let e_const c = EConst c
let e_var x = EVar x
let e_app f x = EApp (f, x)
let e_if_else cond e_true e_false = EIfElse (cond, e_true, e_false)
let e_fun p e = EFun (p, e)
let e_let_in def e = ELetIn (def, e)
let e_tuple exprs = ETuple exprs
let e_list exprs = EList exprs
let e_match e branches = EMatch (e, branches)
<<<<<<< HEAD
||||||| parent of 413c173 (CC from scratch)
=======
let d_let p e = DLet (NonRec, p, e)
let d_let_rec p e = DLet (Rec, p, e)

let d_let_flag = function
  | Rec -> d_let_rec
  | NonRec -> d_let
;;
>>>>>>> 413c173 (CC from scratch)

(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open QCheck

let div = 16

let gen_id =
  let open QCheck.Gen in
  let is_keyword x =
    List.mem x [ "let"; "rec"; "in"; "fun"; "if"; "then"; "else"; "match"; "with" ]
  in
  let id = string_size ~gen:(char_range 'a' 'z') (int_range 1 5) in
  id >>= fun id_ -> if is_keyword id_ then id else return id_
;;

type id = (string[@gen gen_id]) [@@deriving eq, show { with_path = false }, qcheck, eq]

type rec_flag =
  | Rec (** recursive *)
  | NonRec (** non-recursive *)
[@@deriving show { with_path = false }, qcheck, eq]

type constant =
  | CInt of int (** 42 *)
  | CBool of bool (** true | false *)
  | CUnit (** () *)
  | CNil (** [] *)
[@@deriving show { with_path = false }, qcheck, eq]

type type_ann =
  | TAInt (** int *)
  | TABool (** bool *)
  | TAUnit (** () *)
  | TATuple of
      type_ann
      * type_ann
      * (type_ann list[@gen Gen.(list_size (0 -- 4) (gen_type_ann_sized (n / div)))])
  (** (int * bool) *)
  | TAFun of
      (type_ann[@gen Gen.(gen_type_ann_sized (n / div))])
      * (type_ann[@gen Gen.(gen_type_ann_sized (n / div))]) (** int -> bool *)
  | TAList of (type_ann[@gen Gen.(gen_type_ann_sized (n / div))]) (** %type% list *)
[@@deriving show { with_path = false }, qcheck, eq]

type pattern =
  | PConst of constant
  | PWild (** _ *)
  | PIdent of id (** x *)
  | PTuple of
      pattern
      * pattern
      * (pattern list[@gen Gen.(list_size (0 -- 4) (gen_pattern_sized (n / div)))])
  (** (a, b) *)
  | PList of (pattern list[@gen Gen.(list_size (1 -- 4) (gen_pattern_sized (n / div)))])
  (** [1, 2, 3] *)
  | PCons of
      (pattern[@gen Gen.(gen_pattern_sized 0)])
      * (pattern[@gen Gen.(gen_pattern_sized (n / div))]) (** hd :: tl *)
  | PAnn of
      (pattern[@gen Gen.(gen_pattern_sized (n / div))])
      * (type_ann[@gen Gen.(gen_type_ann_sized (n / div))]) (** (x: int) *)
[@@deriving show { with_path = false }, qcheck, eq]

type expr =
  | EConst of constant (** 42, true, ()*)
  | EVar of id (** x *)
  | EApp of
      (expr[@gen Gen.(gen_expr_sized (n / div))])
      * (expr[@gen Gen.(gen_expr_sized (n / div))]) (** f x *)
  | EIfElse of
      (expr[@gen Gen.(gen_expr_sized (n / div))])
      * (expr[@gen Gen.(gen_expr_sized (n / div))])
      * (expr[@gen Gen.(gen_expr_sized (n / div))]) (** if x then y else z *)
  | EFun of
      (pattern list[@gen Gen.(list_size (2 -- 4) (gen_pattern_sized (n / div)))]) * expr
  (** fun x -> y *)
  | ELetIn of definition * expr (** let x = y in z *)
  | ETuple of
      expr * expr * (expr list[@gen Gen.(list_size (0 -- 4) (gen_expr_sized (n / div)))])
  (** (x, fun x -> x, 42) *)
  | EList of (expr list[@gen Gen.(list_size (2 -- 4) (gen_expr_sized (n / div)))])
  (** [1; 2; 3] *)
  | EMatch of
      ((expr * (pattern * expr) list)
      [@gen
        Gen.(
          pair
            (gen_expr_sized (n / div))
            (list_size
               (2 -- 4)
               (pair (gen_pattern_sized (n / div)) (gen_expr_sized (n / div)))))])
  (** match x with ... *)
[@@deriving show { with_path = false }, qcheck, eq]

and definition =
  | DLet of
      rec_flag
      * (pattern[@gen gen_pattern_sized (n / div)])
      * (expr[@gen gen_expr_sized (n / div)]) (** let (rec)? x = y *)
[@@deriving show { with_path = false }, qcheck, eq]

type program = (definition list[@gen Gen.(list_size (1 -- 3) gen_definition)])
[@@deriving show { with_path = false }, qcheck, eq]

let p_const c = PConst c
let p_wild = PWild
let p_ident x = PIdent x
let p_tuple hd1 hd2 tl = PTuple (hd1, hd2, tl)
let p_list ps = PList ps
let p_cons hd tl = PCons (hd, tl)
let p_ann p t = PAnn (p, t)
let e_const c = EConst c
let e_var x = EVar x
let e_app f x = EApp (f, x)
let e_if_else cond e_true e_false = EIfElse (cond, e_true, e_false)
let e_fun p e = EFun (p, e)
let e_let_in def e = ELetIn (def, e)
let e_tuple hd1 hd2 tl = ETuple (hd1, hd2, tl)
let e_list exprs = EList exprs
let e_match e branches = EMatch (e, branches)
let d_let p e = DLet (NonRec, p, e)
let d_let_rec p e = DLet (Rec, p, e)

let d_let_flag = function
  | Rec -> d_let_rec
  | NonRec -> d_let
;;

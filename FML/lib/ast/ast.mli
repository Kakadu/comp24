(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type id = string [@@deriving show { with_path = false }]

type const =
  | CInt of int (** 1 *)
  | CBool of bool (** true *)
[@@deriving show { with_path = false }]

type rec_flag =
  | Rec (** rec *)
  | NoRec (** no rec annotation *)
[@@deriving show { with_path = false }]

type type_annotation =
  | AUnit (** : unit *)
  | AInt (** : int *)
  | ABool (** : bool *)
  | AVar of int (** 'a *)
  | AList of type_annotation (** : 'a list *)
  | ATuple of type_annotation list (** : 'a * 'b *)
  | AFunction of type_annotation * type_annotation (** : 'a -> 'b *)
[@@deriving show { with_path = false }]

type pattern =
  | PNill (** [] *)
  | PAny (**  _ *)
  | PUnit (** () *)
  | PConst of const (** 1 || true *)
  | PIdentifier of id (** x *)
  | PTuple of pattern list (** (x, y, z) *)
  | PCons of pattern * pattern (** hd :: tl*)
  | PConstraint of pattern * type_annotation (** P : type annotation *)
[@@deriving show { with_path = false }]

type expression =
  | EUnit (** () *)
  | ENill (** [] *)
  | EConstraint of expression * type_annotation (** E : type annotation *)
  | EConst of const (** 1 || true*)
  | EIdentifier of id (** x *)
  | EApplication of expression * expression (** E1 E2*)
  | EFun of pattern * expression (** fun P -> E*)
  | ELetIn of rec_flag * pattern * expression * expression (** let f x = E1 *)
  | ETuple of expression list (** (E1, E2, E3) *)
  | EIf of expression * expression * expression (** if e1 then e2 else e3 *)
  | ECons of expression * expression (** [a; b; c], a :: [b; c]*)
  | EMatch of expression * (pattern * expression) list
  (** match e with p1 -> e1 |...| pn -> en *)
[@@deriving show { with_path = false }]

type single_declaration = DDeclaration of pattern * expression (** P = E*)
[@@deriving show { with_path = false }]

type declaration =
  | NoRecDecl of single_declaration list
  (** let single_declaration (and single_declaration) *)
  | RecDecl of single_declaration list
  (** let rec single_declaration (and single_declaration) *)
[@@deriving show { with_path = false }]

(** declaration;; declaration *)
type program = declaration list [@@deriving show { with_path = false }]

(* Constructors for patterns *)
val cint : int -> const
val cbool : bool -> const
val pany : 'a -> pattern
val pnill : 'a -> pattern
val punit : 'a -> pattern
val pident : id -> pattern
val pconst : const -> pattern
val pcons : pattern -> pattern -> pattern
val ptuple : pattern list -> pattern
val pconstraint : pattern -> type_annotation -> pattern

(* Constructors for expressions *)

val econst : const -> expression
val eidentifier : id -> expression
val etuple : expression list -> expression
val efun : pattern -> expression -> expression
val econstraint : expression -> type_annotation -> expression

(* Constructor for declaration *)
val ddeclaration : pattern -> expression -> single_declaration

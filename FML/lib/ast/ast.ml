(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type id = string [@@deriving show { with_path = false }]

type const =
  | CInt of int (** 1 *)
  | CBool of bool (** true *)
  | CNil (** [] *)
[@@deriving show { with_path = false }]

type rec_flag =
  | Rec
  | NoRec
[@@deriving show { with_path = false }]

type type_annotation =
  | AUnit
  | AInt
  | ABool
  | AList of type_annotation
  | ATuple of type_annotation list
  | AFunction of type_annotation * type_annotation
[@@deriving show { with_path = false }]

type pattern =
  | PNill (** [] *)
  | PAny (**  _ *)
  | PUnit (** () *)
  | PConst of const (** 1 || true *)
  | PIdentifier of id (** x *)
  | PTuple of pattern list (** (x, y, z) *)
  | PCons of pattern * pattern (** hd :: tl*)
  | PConstraint of pattern * type_annotation
[@@deriving show { with_path = false }]

type un_op =
  | Not (** not *)
  | Minus (** - *)
  | Plus (** +*)
[@@deriving show { with_path = false }]

type expression =
  | EConst of const
  | EIdentifier of id
  | EApplication of expression * expression (** E1 E2*)
  | EFun of pattern * expression (** fun P -> E*)
  | ELetIn of rec_flag * pattern * expression * expression (** let f x = E1 *)
  | ETuple of expression list
  | EIf of expression * expression * expression (** if e1 then e2 else e3 *)
  | ECons of expression * expression (** [a; b; c], a :: [b; c]*)
  | EMatch of expression * (pattern * expression) list
  (** match e with p1 -> e1 |...| pn -> en *)
[@@deriving show { with_path = false }]

type declaration = DDeclaration of rec_flag * pattern * expression
[@@deriving show { with_path = false }]

type program = declaration list [@@deriving show { with_path = false }]

let cint x = CInt x
let cbool x = CBool x

(* Constructors for patterns *)
let pany _ = PAny
let pnill _ = PNill
let punit _ = PUnit
let pconst c = PConst c
let pident v = PIdentifier v
let pcons l r = PCons (l, r)
let ptuple l = PTuple l
let pconstraint p t = PConstraint (p, t)

(* ------------------------- *)

(* Constructors for expressions *)

let econst c = EConst c
let eidentifier v = EIdentifier v
let etuple l = ETuple l
let efun arg expr = EFun (arg, expr)

(* ---------------------------- *)

(* Constructor for declaration *)
let ddeclaration rec_f pat expr = DDeclaration (rec_f, pat, expr)

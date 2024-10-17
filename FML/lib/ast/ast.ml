(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type id = string [@@deriving show { with_path = false }]

type const =
  | CInt of int (** 1 *)
  | CBool of bool (** true *)
[@@deriving show { with_path = false }]

type rec_flag =
  | Rec
  | NoRec
[@@deriving show { with_path = false }]

type pattern =
  | PAny (**  _ *)
  | PConst of const (** 1 || true *)
  | PIdentifier of id (** x *)
  | PTuple of pattern list (** (x, y, z) *)
  | PNill (** [] *)
  | PCons of pattern * pattern (** hd :: tl*)
[@@deriving show { with_path = false }]

type bin_op =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
  | Eq (** = *)
  | NEq (** <> or != *)
  | Gt (** > *)
  | Gte (** >= *)
  | Lt (** < *)
  | Lte (** <= *)
  | And (** && *)
  | Or (** || *)
[@@deriving show { with_path = false }]

type un_op =
  | Not (** not *)
  | Minus (** - *)
  | Plus (** +*)
[@@deriving show { with_path = false }]

type expression =
  | EConst of const
  | EIdentifier of id
  | EBinaryOperation of bin_op * expression * expression (** E1 + E2*)
  | EUnaryOperation of un_op * expression (** - E1 *)
  | EApplication of expression * expression (** E1 E2*)
  | EFun of pattern * expression (** fun P -> E*)
  | ELetIn of rec_flag * pattern * expression * expression (** let f x = E1 *)
  | ETuple of expression list
[@@deriving show { with_path = false }]

type declaration = rec_flag * pattern * expression [@@deriving show { with_path = false }]
type program = declaration list [@@deriving show { with_path = false }]

let cint x = CInt x
let cbool x = CBool x

(* Constructors for patterns *)
let pany _ = PAny
let pnill _ = PNill
let pconst c = PConst c
let pident v = PIdentifier v
let pcons l r = PCons (l, r)
let ptuple l = PTuple l

(* ------------------------- *)

(* Constructors for expressions *)

let econst c = EConst c
let eidentifier v = EIdentifier v
let etuple l = ETuple l

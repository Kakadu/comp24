(** Copyright 2024-2025, Perevalov Efim, Dyachkov Vitaliy *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving eq, show { with_path = false }]

type const =
  | CInt of int (** integer number: ..., 0, 1, 2, ...*)
  | CBool of bool (** boolean values: true and false *)
  | CNil (** [] *)
[@@deriving eq, show { with_path = false }]

type binary_op =
  | Add (** 1 + 2 *)
  | Sub (** 2 - 1 *)
  | Mul (** * *)
  | Div (** / *)
  | And (** && *)
  | Or (** || *)
  | Eq (** = *)
  | Neq (** <> *)
  | Less (** < *)
  | Gre (** > *)
  | Leq (** <= *)
  | Greq (** >= *)
[@@deriving eq, show { with_path = false }]

type type_of_var =
  | TInt (** int type for variable *)
  | TBool (** bool type for variable *)
  | TUnknown (** unknown type for variable *)
  | TArrow of type_of_var * type_of_var (** type int -> int... *)
[@@deriving eq, show { with_path = false }]

type pattern =
  | PWild (** _ *)
  | PConst of const (** constant pattern *)
  | PVar of name * type_of_var (** variable pattern*)
  | PTuple of pattern list (** (a, b) *)
  | PCon of pattern * pattern (** hd :: tl *)
[@@deriving eq, show { with_path = false }]

type rec_flag =
  | Rec (** let rec v = ... *)
  | Notrec (** let z = ...*)
[@@deriving eq, show { with_path = false }]

type expression =
  | EConst of const (** constant *)
  | EVar of name * type_of_var (** variable *)
  | EBinaryOp of binary_op * expression * expression (** binary operation *)
  | EApp of expression * expression * type_of_var(** application *)
  | EIfElse of expression * expression * expression (** if z then v else n*)
  | ELetIn of rec_flag * name list * expression * expression
  | EFun of pattern * expression (** fun z -> z + z *)
  | EList of expression * expression (** [1;2;3]*)
  | ETuple of expression list (** (1,2,3) *)
  | EMatch of expression * (pattern * expression) list
[@@deriving eq, show { with_path = false }]

(** Binding type *)
type bindings =
  | Let of (rec_flag * name list * expression) list (** let id = expr *)
  | Expression of expression (** simple expressions *)
[@@deriving show { with_path = false }]

(** Statements type *)
type statements = bindings list [@@deriving show { with_path = false }]
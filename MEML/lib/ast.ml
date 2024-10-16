(** Copyright 2024-2025, Perevalov Efim, Ermolovich Anna *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving show { with_path = false }]

type const =
  | CInt of int (** integer number: ..., 0, 1, 2, ...*)
  | CString of string (** string values: "Ocaml" *)
  | CBool of bool (** boolean values: true and false *)
[@@deriving show { with_path = false }]

type binary_op =
  | Add (** 1 + 2 *)
  | Sub (** 1 - 2 *)
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
[@@deriving show { with_path = false }]

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
[@@deriving show { with_path = false }]

type expression =
  | EConst of const (** constant *)
  | EVar of name (** variable *)
  | EBinaryOp of binary_op * expression * expression (** binary operation *)
  | EApp of expression * expression * type_of_var (** application *)
  | EIfElse of expression * expression * expression (** if z then v else n*)
  | ELetIn of rec_flag * name * expression * expression
  | ELetPatIn of pattern * expression * expression
  | EFun of pattern * expression (** fun z -> z + z *)
  | EList of expression * expression (** [1;2;3]*)
  | ETuple of expression list (** (1,2,3) *)
  | EMatch of expression * (pattern * expression) list
[@@deriving eq, show { with_path = false }]

(** Binding type *)
type bindings =
  | Let of rec_flag * (pattern * expression) list
  | Expression of expression (** simple expressions *)
[@@deriving eq, show { with_path = false }]

(** Statements type *)
type statements = bindings list [@@deriving eq, show { with_path = false }]

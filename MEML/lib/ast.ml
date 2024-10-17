(** Copyright 2024-2025, Perevalov Efim, Dyachkov Vitaliy *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type name = string [@@deriving show { with_path = false }]

type const =
  | CInt of int (** integer number: ..., 0, 1, 2, ...*)
  | CString of string (** string values: "Ocaml" *)
  | CBool of bool (** boolean values: true and false *)
[@@deriving show { with_path = false }]

type binary_op =
  | Add (** + *)
  | Sub (** - *)
  | Mul (** * *)
  | Div (** / *)
  | Mod (** % *)
  | And (** && *)
  | Or (** || *)
  | Eq (** = *)
  | Neq (** <> *)
  | Less (** < *)
  | Gre (** > *)
  | Leq (** <= *)
  | Greq (** >= *)
[@@deriving show { with_path = false }]

type type_of_var = 
  | TInt
  | TBool
  | TString
  | TUnknown 
[@@deriving show { with_path = false }]

type pattern =
  | PWild (** _ *)
  | PConst of const (** constant pattern *)
  | PVar of name * type_of_var (** variable pattern*)
[@@deriving show { with_path = false }]

type rec_flag = 
  | Rec (** let rec v = ... *)
  | Notrec (** let z = ...*)
[@@deriving show { with_path = false }]

type expression =
  | EConst of const (** constant *)
  | EVar of name * type_of_var (** variable *)
  | EBinaryOp of binary_op * expression * expression (** binary operation *)
  | EApp of expression * expression (** application *)
  | EIfElse of expression * expression * expression (** if z then v else n*)
  | ELetIn of rec_flag * name * expression * expression
  | EFun of pattern * expression (** fun z -> z + z *)
[@@deriving show { with_path = false }]

(** Binding type *)
type bindings =
  | Let of rec_flag * name * expression (** An expression for let declaration: let id = expr *)
  | Expression of expression
  (** An expression for let rec declaration: let rec id = expr *)
[@@deriving show { with_path = false }]

(** Statements type *)
type statements = bindings list [@@deriving show { with_path = false }]
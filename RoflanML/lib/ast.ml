(** Copyright 2021-2023, Ilya Syresenkov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Identifiers for variables and functions names *)
type id = string [@@deriving show { with_path = false }]

(** Binary operators *)
type binop =
  | Add (** [+] operator *)
  | Sub (** [-] operator *)
  | Div (** [/] operator *)
  | Mul (** [*] operator *)
  | Eq (** [=] operator *)
  | Neq (** [<>] operator *)
  | Les (** [<] operator *)
  | Leq (** [<=] operator *)
  | Gre (** [>] operator *)
  | Geq (** [>=] operator *)
[@@deriving show { with_path = false }]

type const =
  | CInt of int (** Integers 1, 2, ... *)
  | CBool of bool (** Boolean true or false *)
  | CUnit (** Unit () *)
[@@deriving show { with_path = false }]

type pattern =
  | PWild (** | _ -> ... *)
  | PEmpty (** | [] -> ... *)
  | PConst of const (** | const -> ... *)
  | PVar of id (** | varname -> ... *)
  | PCons of pattern * pattern * pattern list (** | p1 :: p2 -> ... *)
  | POr of pattern * pattern * pattern list (** | p1 | p2 | p3 -> ... *)
[@@deriving show { with_path = false }]

type is_rec =
  | Rec (** Recursive functions tag, can call themselves in their body *)
  | NonRec (** Non-recursive functions tag *)
[@@deriving show { with_path = false }]

type type_id =
  | TInt (** Type int *)
  | TBool (** Type bool *)
  | TUnit (** Type unit () *)
  | TFun of type_id * type_id (** Function type t1 -> t2 *)
  | TList of type_id (** List type t list *)
[@@deriving show { with_path = false }]

type expr =
  | EConst of const (** Consts *)
  | EVar of id (** Variables with their names *)
  | EBinop of binop * expr * expr (** e1 binop e2 *)
  | ETuple of expr * expr * expr list
  (** Tuples of 2 or more elements, separated by ',' *)
  | EList of expr list (** Lists [1; 2; 3], ... *)
  | EBranch of expr * expr * expr (** if [cond] then [a] else [b] *)
  | EMatch of expr * (pattern * expr) list (** match [x] with | [p1] -> [e1] | ... *)
  | ELet of is_rec * (id * type_id) list * expr * expr option (** let rec f: t1 -> t2 *)
  | EFun of (id * type_id) list * expr (** Anonymous function with typed arguments *)
  | EApp of expr * expr (** Application f x *)
[@@deriving show { with_path = false }]

type program = expr list [@@deriving show { with_path = false }]
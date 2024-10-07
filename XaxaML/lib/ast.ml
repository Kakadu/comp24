(** Copyright 2024, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type const =
  | Int of int (** 1 *)
  | Bool of bool (** true *)
[@@deriving show { with_path = false }]

type typ =
  | TVar of int (** type var *)
  | TPrim of string (** ground type *)
  | TArr of typ * typ (** function type *)
  | TUnit (** unit *)
  | TTuple of typ * typ list (** tuple type *)
  | TList of typ (** list type *)
[@@deriving show { with_path = false }]

type pattern =
  | Pat_any (** _ *)
  | Pat_val of string (** abc *)
  | Pat_const of const (** 1 *)
  | Pat_tuple of pattern * pattern list (** (1, 2) *)
  | Pat_cons_list of pattern * pattern (** 1::[] or [1;2] *)
  | Pat_empty_list (** [] *)
[@@deriving show { with_path = false }]

type arg = pattern * typ option [@@deriving show { with_path = false }]

type expr =
  | Expr_const of const (** 1 *)
  | Expr_val of string (** val1 *)
  | Expr_ite of expr * expr * expr (** if a then b else c *)
  | Expr_fun of arg list * expr (** fun x -> x + 1 TODO: invariant *)
  | Expr_app of expr * expr list (** f x y *)
  | Expr_let of decl * expr (** let a = 1 in b *)
  | Expr_match of expr * (pattern * expr) list (** match x with | 1 -> 0 | _ -> 1 *)
  | Expr_empty_list (** [] *)
  | Expr_cons_list of expr * expr (** 1::[] or [1;2] *)
  | Expr_tuple of expr * expr list (** (1, a, 'c') *)
[@@deriving show { with_path = false }]

and decl = bool * string * typ option * expr * typ option (* TODO: explain *)
[@@deriving show { with_path = false }]

type toplevel =
  | Let_decl of decl (** let a = 1 *)
  | Expr of expr (** printf "abc" *)
[@@deriving show { with_path = false }]

type program = toplevel list [@@deriving show { with_path = false }]

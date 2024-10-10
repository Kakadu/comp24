(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type const =
  | C_int of int (** 1 *)
  | C_bool of bool (** true *)
  | C_empty_list (** [] *)
  | C_unit (** () *)
[@@deriving show { with_path = false }]

type raw_typ =
  | RT_var of string (** type var *)
  | RT_prim of string (** ground type *)
  | RT_arr of raw_typ * raw_typ (** function type *)
  | RT_tuple of raw_typ * raw_typ list (** tuple type *)
  | RT_list of raw_typ (** list type *)
[@@deriving show { with_path = false }]

type pattern =
  | P_typed of pattern * raw_typ (** (a: int) *)
  | P_any (** _ *)
  | P_val of string (** abc *)
  | P_const of const (** 1 *)
  | P_tuple of pattern * pattern list (** (1, 2) *)
  | P_cons_list of pattern * pattern (** 1::[] or [1;2] *)
[@@deriving show { with_path = false }]

type expr =
  | E_typed of expr * raw_typ (** val1 : int *)
  | E_const of const (** 1 *)
  | E_ident of string (** val1 or ( + ) *)
  | E_ite of expr * expr * expr (** if a then b else c *)
  | E_fun of pattern * pattern list * expr (** fun x -> x + 1 *)
  | E_app of expr * expr (** f x *)
  | E_let of decl * expr (** let a = 1 in b *)
  | E_match of expr * (pattern * expr) list (** match x with 1 -> 0 | _ -> 1 *)
  | E_cons_list of expr * expr (** 1::[] or [1;2] *)
  | E_tuple of expr * expr list (** (1, a) *)
[@@deriving show { with_path = false }]

and decl_body = pattern * raw_typ option * expr

and decl =
  | Non_rec of decl_body (** let x = 1 *)
  | Rec of decl_body list (** let rec a = ... and b = ... *)

type toplevel =
  | Let_decl of decl (** let a = 1 *)
  | Expr of expr (** a + b *)
[@@deriving show { with_path = false }]

type program = toplevel list [@@deriving show { with_path = false }]

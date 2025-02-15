(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type llexpr =
  | LLConst of const (** Consts *)
  | LLVar of id (** Variables with their names *)
  | LLTuple of llexpr * llexpr * llexpr list
  (** Tuples of 2 or more elements, separated by ',' *)
  | LLList of llexpr list (** Lists [1; 2; 3], ... *)
  | LLBranch of llexpr * llexpr * llexpr (** if [cond] then [a] else [b] *)
  | LLMatch of llexpr * (pattern * llexpr) list (** match [x] with | [p1] -> [e1] | ... *)
  | LLLetIn of is_rec * id * llexpr * llexpr (** let rec f: t1 -> t2 *)
  | LLApp of llexpr * llexpr (** Application f x y z *)
[@@deriving show { with_path = false }]

type lldecl = LLDLet of is_rec * id * typed_arg list * llexpr
[@@deriving show { with_path = false }]

type llprogram = lldecl list [@@deriving show { with_path = false }]

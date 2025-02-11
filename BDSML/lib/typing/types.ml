(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type type_var_id = int [@@deriving show { with_path = false }]

type base_type =
  | Int
  | Char
  | String
[@@deriving show { with_path = false }]

type type_val =
  | Type_var of type_var_id (** e.g. ['a] *)
  | Type_base of base_type (** e.g. [int] *)
  | Type_params of type_val * type_val (** e.g. [int list] *)
  | Type_tuple of type_val list (** e.g. [int * int] *)
  | Type_fun of type_val list (** e.g. [int -> int] *)
[@@deriving show { with_path = false }]

type scheme = Set.Make(Int).t * type_val

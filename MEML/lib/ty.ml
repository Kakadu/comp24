(** Copyright 2024-2025, Perevalov Efim, Ermolovich Anna *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binder = int [@@deriving eq, show { with_path = false }]

module VarSet = struct
  include Stdlib.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type binder_set = VarSet.t [@@deriving show { with_path = false }]

type ty =
  | TPrim of string
  | TVar of binder
  | TArrow of ty * ty
  | TList of ty
  | TTuple of ty list
[@@deriving eq, show { with_path = false }]

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of ty * ty
  | `Not_solo_var
  | `Bad_let
  ]

type scheme = S of binder_set * ty [@@deriving show { with_path = false }]

let arrow l r = TArrow (l, r)
let int_typ = TPrim "int"
let bool_typ = TPrim "bool"
let unit_typ = TPrim "unit"
let tuple_typ t = TTuple t
let list_typ t = TList t

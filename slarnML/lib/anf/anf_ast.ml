(** Copyright 2024-2025, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type immexpr =
  | AId of string
  | AInt of int
  | ABool of bool
  | AUnit
[@@deriving show { with_path = false }]

type cexpr =
  | ANot of immexpr
  | AOr of immexpr * immexpr
  | AAnd of immexpr * immexpr
  | AEq of immexpr * immexpr
  | AGt of immexpr * immexpr
  | ALt of immexpr * immexpr
  | AGte of immexpr * immexpr
  | ALte of immexpr * immexpr
  | AAdd of immexpr * immexpr
  | ASub of immexpr * immexpr
  | AMul of immexpr * immexpr
  | ADiv of immexpr * immexpr
  | CImmExpr of immexpr
  | AIf of immexpr * aexpr * aexpr
  | AApp of immexpr * immexpr list
[@@deriving show { with_path = false }]

and aexpr =
  | ALet of string * cexpr * aexpr
  | ACExpr of cexpr
[@@deriving show { with_path = false }]

type afun = AFun of string * string list * aexpr [@@deriving show { with_path = false }]

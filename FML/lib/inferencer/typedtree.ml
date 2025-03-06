(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type type_var = int

type typ =
  | TBool
  | TInt
  | TUnit
  | TVar of type_var
  | TFunction of typ * typ
  | TTuple of typ list
  | TList of typ

module TVarSet = Stdlib.Set.Make (Int)
module VarSet = Stdlib.Set.Make (String)

type scheme = Scheme of TVarSet.t * typ

let tint = TInt
let tbool = TBool
let tunit = TUnit
let tvar t = TVar t
let tfunction l r = TFunction (l, r)
let ttuple t = TTuple t
let tlist t = TList t

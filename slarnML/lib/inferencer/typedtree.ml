(** Copyright 2023-2025, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type binder = int [@@deriving show { with_path = false }]

module VarSet = struct
  include Stdlib.Set.Make (Int)
end

type constTy =
  | UnitTy
  | IntTy
  | BoolTy

type ty =
  | PrimTy of constTy
  | VarTy of binder
  | ArrowTy of ty list
[@@deriving show { with_path = false }]

type undefinedType =
  | Type of ty
  | Key of binder
  | Undefined

open Res

let boolType = Result (Type (PrimTy BoolTy))
let uniyType = Result (Type (PrimTy UnitTy))
let intType = Result (Type (PrimTy IntTy))

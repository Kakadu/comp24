(** Copyright 2023-2025, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

type ty =
  | TVar of tv ref
  | TInt
  | TBool
  | TUnit
  | TFun of ty * ty

and tv =
  | Unbound of int * int
  | Link of ty

type context = (string * ty) list
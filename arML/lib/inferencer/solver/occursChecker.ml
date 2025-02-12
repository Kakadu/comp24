(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open TypeTree

(*** Checks whether the passed type variable is contained in the passed type. *)
let rec occurs_check v = function
  | TVar b -> b = v
  | TArr (left, right) -> occurs_check v left || occurs_check v right
  | TList typ -> occurs_check v typ
  | TTuple typ_list ->
    List.fold_left (fun acc item -> acc || occurs_check v item) false typ_list
  | TGround _ -> false
;;

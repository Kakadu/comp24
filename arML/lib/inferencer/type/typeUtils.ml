(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open TypeTree

(* Return all type variables in type *)
let type_vars =
  let rec helper acc = function
    | TVar n -> TypeVarSet.add n acc
    | TArr (left, right) -> helper (helper acc left) right
    | TList typ -> helper acc typ
    | TTuple typ_list -> List.fold_left helper acc typ_list
    | TGround _ -> acc
  in
  helper TypeVarSet.empty
;;

(* Syntactic sugar *)

let (@->) left right = TArr (left, right)

(* ---------------- *)

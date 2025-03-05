(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open TypeTree

type schema = Schema of TypeVarSet.t * typ

let free_vars = function
  | Schema (bind_vars, ty) -> TypeVarSet.diff (TypeUtils.type_vars ty) bind_vars
;;

let apply sub = function
  | Schema (bind_vars, ty) ->
    let sub2 =
      TypeVarSet.fold (fun sub key -> Substitution.remove key sub) bind_vars sub
    in
    Schema (bind_vars, Substitution.apply sub2 ty)
;;

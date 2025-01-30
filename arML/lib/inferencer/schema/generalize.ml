(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open TypeTree

let generalize : TypeEnv.t -> typ -> Schema.schema =
  fun env ty ->
  let free = TypeVarSet.diff (TypeUtils.type_vars ty) (TypeEnv.free_vars env) in
  Schema (free, ty)
;;

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open StateResultMonad
open StateResultMonad.Syntax
open TypeTree

let instantiate : Schema.schema -> TypeTree.typ StateResultMonad.t =
  fun (Schema (bind_var, ty)) ->
  TypeVarSet.fold
    (fun var_name acc ->
       let* acc = acc in
       let* fv = fresh >>| fun name -> TVar name in
       let* sub = Substitution.singleton var_name fv in
       return (Substitution.apply sub acc))
    bind_var
    (return ty)
;;

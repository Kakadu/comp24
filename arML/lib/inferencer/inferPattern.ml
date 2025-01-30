(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open StateResultMonad
open StateResultMonad.Syntax
open CommonFunctions
open TypeUtils

let infer_pattern =
  let rec helper env = function
    | Ast.PConst c -> return (infer_const c, env)
    | Ast.PVar (Id v) ->
      let* fv = fresh_var in
      let schema = Schema.Schema (TypeTree.TypeVarSet.empty, fv) in
      let env = TypeEnv.extend env v schema in
      return (fv, env)
    | _ ->
      (* !!! *)
      let* fv = fresh_var in
      return (fv, env)
  in
  helper
;;

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open StateResultMonad
open StateResultMonad.Syntax
open CommonFunctions
open UniquePatternVarsChecker
open TypeUtils

let infer_pattern =
  let rec helper env = function
    | Ast.PConst c -> return (infer_const c, env)
    | Ast.PVar (Id v) ->
      let* fv = fresh_var in
      let schema = Schema.Schema (TypeTree.TypeVarSet.empty, fv) in
      let env = TypeEnv.extend env v schema in
      return (fv, env)
    | Ast.PAny ->
      let* fv = fresh_var in
      return (fv, env)
    | Ast.PNill ->
      let* fv = fresh_var in
      let ty = TypeTree.TList fv in
      return (ty, env)
    | Ast.PTuple (first_pattern, second_pattern, patterns) as tuple_p ->
      let* _ = check_unique_vars tuple_p in
      (* Check several bounds *)
      let* ty, env =
        (* Here is the list of types in reverse order. *)
        RList.fold_left
          (first_pattern :: second_pattern :: patterns)
          ~init:(return ([], env))
          ~f:(fun (acc, env) (pattern) ->
            let* ty1, env1 = helper env pattern in
            return (ty1 :: acc, env1))
      in
      let ty = TypeTree.TTuple (List.rev ty) in
      return (ty, env)
    | Ast.PTyped (pattern, pattern_ty) ->
      let* ty, env = helper env pattern in
      let pattern_ty = get_type_by_annotation pattern_ty in
      let* sub = Substitution.unify ty pattern_ty in
      return (Substitution.apply sub ty, TypeEnv.apply env sub)
    | _ ->
      (* !!! *)
      let* fv = fresh_var in
      return (fv, env)
  in
  helper
;;

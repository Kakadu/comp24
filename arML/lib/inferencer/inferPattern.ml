(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateResultMonad
open Common.StateResultMonad.Syntax
open UniquePatternVarsChecker
open InferBasic
open Ast.AbstractSyntaxTree

let infer_pattern =
  let rec helper env = function
    | PConst c -> return (infer_const c, env)
    | PVar (Id v) ->
      let* fv = fresh_var in
      let schema = Schema.Schema (TypeTree.TypeVarSet.empty, fv) in
      let env = TypeEnv.extend env v schema in
      return (fv, env)
    | PAny ->
      let* fv = fresh_var in
      return (fv, env)
    | PNill ->
      let* fv = fresh_var in
      let ty = TypeTree.TList fv in
      return (ty, env)
    | PTuple (first_pattern, second_pattern, patterns) as tuple_p ->
      let* _ = check_unique_vars [ tuple_p ] in
      (* Check several bounds *)
      let* ty, env =
        RList.fold_left
          (first_pattern :: second_pattern :: patterns)
          ~init:(return ([], env))
          ~f:(fun (acc, env) (pattern) ->
              let* ty', env' = helper env pattern in
              return (ty' :: acc, env'))
      in
      let ty = TypeTree.TTuple (List.rev ty) in
      return (ty, env)
    | PListConstructor (left, right) as list_cons ->
      let* _ = check_unique_vars [ list_cons ] in
      (* Check several bounds *)
      let* ty1, env' = helper env left in
      let* ty2, env'' = helper env' right in
      let* fv = fresh_var in
      let* sub = Substitution.unify (TList ty1) fv in
      let* sub' = Substitution.unify ty2 fv in
      let* sub'' = Substitution.compose sub sub' in
      let env''' = TypeEnv.apply env'' sub'' in
      let ty3 = Substitution.apply sub'' fv in
      return (ty3, env''')
    | PTyped (pattern, pattern_ty) ->
      let* ty, env = helper env pattern in
      let* expected_ty = get_type_by_defenition pattern_ty in
      let* sub = Substitution.unify ty expected_ty in
      return (Substitution.apply sub ty, TypeEnv.apply env sub)
  in
  helper
;;

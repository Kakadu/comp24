(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateResultMonad
open Common.StateResultMonad.Syntax
open UniquePatternVarsChecker
open InferBasic

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
      let* _ = check_unique_vars [ tuple_p ] in
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
    | Ast.PListConstructor (left, right) as list_cons ->
      let* _ = check_unique_vars [ list_cons ] in
      (* Check several bounds *)
      let* ty1, env1 = helper env left in
      let* ty2, env2 = helper env1 right in
      let* fv = fresh_var in
      let* sub1 = Substitution.unify (TList ty1) fv in
      let* sub2 = Substitution.unify ty2 fv in
      let* sub3 = Substitution.compose sub1 sub2 in
      let env = TypeEnv.apply env2 sub3 in
      let ty3 = Substitution.apply sub3 fv in
      return (ty3, env)
    | Ast.PTyped (pattern, pattern_ty) ->
      let* ty, env = helper env pattern in
      let* expected_ty = get_type_by_annotation pattern_ty in
      let* sub = Substitution.unify ty expected_ty in
      return (Substitution.apply sub ty, TypeEnv.apply env sub)
  in
  helper
;;

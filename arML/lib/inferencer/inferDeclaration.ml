(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open StateResultMonad
open StateResultMonad.Syntax
open CommonFunctions
open InferExpression
open TypeErrors
open TypeTree

let update_name_list name names_list = name :: List.filter (( <> ) name) names_list

let infer_declaration env name_list = function
  | Ast.DOrdinary (pattern, expr) -> 
    let* _ = UniquePatternVarsChecker.check_unique_vars pattern in
    let* _, ty = infer_expr env expr in

    let rec extend_env_with_pattern env name_list pat ty =
      (match pat, ty with
      | Ast.PVar (Id v), ty ->
        let generalized_ty = Generalize.generalize env ty in
        let new_names_list = update_name_list v name_list in
        return ((TypeEnv.extend env v generalized_ty), new_names_list)
      | Ast.PTuple (p1, p2, ps), TypeTree.TTuple ts ->
        (match ts with
        | t1 :: t2 :: rest when List.length rest = List.length ps ->
          let* env, name_list = extend_env_with_pattern env name_list p1 t1 in
          let* env, name_list = extend_env_with_pattern env name_list p2 t2 in
          List.fold_left2 (fun acc pat ty ->
            let* env, name_list = acc in
            extend_env_with_pattern env name_list pat ty
          ) (return (env, name_list)) ps rest
        | _ -> fail Occurs_check) (* !!! *)
      | Ast.PAny, _ -> return (env, name_list)
      | Ast.PNill, TypeTree.TList _ -> return (env, name_list)
      | _ -> fail Occurs_check) (* !!! *)
    in
    let* extended_env, extend_name_list = extend_env_with_pattern env name_list pattern ty in

    return (extended_env, extend_name_list)
  | Ast.DRecursive (pattern, expr) ->
    match pattern with
    | Ast.PVar (Id name) -> 
      let* fv = fresh_var in
      let env2 = TypeEnv.extend env name (Schema.Schema (TypeVarSet.empty, fv)) in
      let* sub1, ty1 = infer_expr env2 expr in
      let* sub2 = Substitution.unify ty1 fv in
      let* sub3 = Substitution.compose sub1 sub2 in
      let ty3 = Substitution.apply sub3 fv in
      let new_acc = TypeEnv.extend env name (Schema.Schema (TypeVarSet.empty, ty3)) in
      let new_names_list = update_name_list name name_list in
      return (new_acc, new_names_list)
    | _ -> fail InvalidRecursionLeftHand
;;

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateResultMonad
open Common.StateResultMonad.Syntax
open TypeTree
open TypeErrors
open InferBasic
open InferPattern
open InferExpression
open Ast.AbstractSyntaxTree

let update_name_list name names_list = name :: List.filter (( <> ) name) names_list

let infer_declaration env name_list = function
  | DOrdinary (case, cases) ->
    let cases = case :: cases in
    let rec extend_env_with_pattern env name_list = function
      | PVar (Id v), ty ->
        let generalized_ty = Generalize.generalize env ty in
        let new_names_list = update_name_list v name_list in
        return (TypeEnv.extend env v generalized_ty, new_names_list)
      | (PTuple (p1, p2, ps) as pat), (TypeTree.TVar _ as ty) ->
        let* tvs =
          List.fold_left
            (fun acc _ ->
              let* acc = acc in
              let* fv = fresh_var in
              return @@ (fv :: acc))
            (return [])
            (p1 :: p2 :: ps)
        in
        let new_ty = TTuple tvs in
        let* sub = Substitution.unify ty new_ty in
        extend_env_with_pattern (TypeEnv.apply env sub) name_list (pat, new_ty)
      | PTuple (p1, p2, ps), TypeTree.TTuple (t1 :: t2 :: rest)
        when List.length rest = List.length ps ->
        let* env, name_list = extend_env_with_pattern env name_list (p1, t1) in
        let* env, name_list = extend_env_with_pattern env name_list (p2, t2) in
        List.fold_left2
          (fun acc pat ty ->
            let* env, name_list = acc in
            extend_env_with_pattern env name_list (pat, ty))
          (return (env, name_list))
          ps
          rest
      | (PListConstructor _ as pat), (TypeTree.TVar _ as ty) ->
        let* fv = fresh_var in
        let new_ty = TList fv in
        let* sub = Substitution.unify ty new_ty in
        extend_env_with_pattern (TypeEnv.apply env sub) name_list (pat, new_ty)
      | PListConstructor (l, r), TypeTree.TList t ->
        let* env, name_list = extend_env_with_pattern env name_list (l, t) in
        extend_env_with_pattern env name_list (r, TypeTree.TList t)
      | PAny, _ | PNill, TypeTree.TList _ -> return (env, name_list)
      | (PConst _ as pat), pty ->
        let* pt, _ = infer_pattern env pat in
        let* _ = Substitution.unify pt pty in
        return (env, name_list)
      | pat, ty ->
        let* typ, _ = infer_pattern env pat in
        fail @@ Unification_failed (typ, ty)
    in
    (* Check several bounds *)
    let patterns = List.map fst cases in
    let* _ = UniquePatternVarsChecker.check_unique_vars patterns in
    List.fold_left
      (fun acc (pat, expr) ->
        let expr =
          match expr with
          | ETyped (EFun (ps, body), typ) -> EFun (ps, ETyped (body, typ))
          | _ -> expr
        in
        let* extended_env, name_list = acc in
        (* We specifically count in a “pure” env.
           This is not a mistake. There is no mutual recursion,
           each declaration in and cannot use the others.*)
        let* _, ty_expr = infer_expr env expr in
        let* extended_env, extend_name_list =
          extend_env_with_pattern extended_env name_list (pat, ty_expr)
        in
        return (extended_env, extend_name_list))
      (return (env, name_list))
      cases
  | DRecursive (case, cases) ->
    let cases = case :: cases in
    (* Check several bounds *)
    let patterns = List.map fst cases in
    let* _ = UniquePatternVarsChecker.check_unique_vars patterns in
    let add_temporary_vars env cases =
      List.fold_left
        (fun acc (pat, _) ->
          let* env, vars = acc in
          match pat with
          | PVar (Id name) ->
            let* fv = fresh_var in
            let env' = TypeEnv.extend env name (Schema.Schema (TypeVarSet.empty, fv)) in
            return (env', (name, fv) :: vars)
          | _ -> fail InvalidRecursionLeftHand)
        (return (env, []))
        cases
    in
    let process_cases env cases temp_vars =
      List.fold_left
        (fun acc -> function
          | PVar (Id name), expr ->
            let expr =
              match expr with
              | ETyped (EFun (ps, body), typ) -> EFun (ps, ETyped (body, typ))
              | _ -> expr
            in
            let* acc_env, acc_name_list = acc in
            let* sub', ty_expr = infer_expr env expr in
            let* tv =
              match List.assoc_opt name temp_vars with
              | Some temp_ty -> return temp_ty
              | None -> fail (Unbound_variable name)
            in
            let* sub'' = Substitution.unify tv ty_expr in
            let* final_sub = Substitution.compose sub' sub'' in
            let final_typ = Substitution.apply final_sub tv in
            let new_acc_env =
              TypeEnv.extend acc_env name (Schema (TypeVarSet.empty, final_typ))
            in
            let new_acc_names_list = update_name_list name acc_name_list in
            return (new_acc_env, new_acc_names_list)
          | _ -> fail InvalidRecursionLeftHand)
        (return (env, name_list))
        cases
    in
    let* env_with_vars, temp_vars = add_temporary_vars env cases in
    process_cases env_with_vars cases temp_vars
;;

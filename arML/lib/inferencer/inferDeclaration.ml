(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open StateResultMonad
open StateResultMonad.Syntax
open TypeTree
open TypeErrors
open InferBasic
open InferPattern
open InferExpression

let update_name_list name names_list = name :: List.filter (( <> ) name) names_list

let infer_declaration env name_list = function
  | Ast.DOrdinary (case, cases) ->
    let cases = case :: cases in

    let rec extend_env_with_pattern env name_list pat ty =
      (match pat, ty with
      | Ast.PVar (Id v), ty ->
        let generalized_ty = Generalize.generalize env ty in
        let new_names_list = update_name_list v name_list in
        return ((TypeEnv.extend env v generalized_ty), new_names_list)
      | Ast.PTuple (p1, p2, ps), TypeTree.TTuple (t1 :: t2 :: rest) when List.length rest = List.length ps ->
          let* env, name_list = extend_env_with_pattern env name_list p1 t1 in
          let* env, name_list = extend_env_with_pattern env name_list p2 t2 in
          List.fold_left2 (fun acc pat ty ->
            let* env, name_list = acc in
            extend_env_with_pattern env name_list pat ty
          ) (return (env, name_list)) ps rest
      | Ast.PAny, _
      | Ast.PNill, TypeTree.TList _ 
      | Ast.PConst(CUnit), TypeTree.TGround(GTUnit) -> return (env, name_list)
      | pat, ty -> 
        let* typ, _ = infer_pattern env pat in
        fail @@ Unification_failed (typ, ty))
    in

    (* Check several bounds *)
    let patterns = List.map fst cases in
    let* _ = UniquePatternVarsChecker.check_unique_vars patterns in

    List.fold_left (fun acc (pat, expr) ->
      let* extended_env, name_list = acc in
      (* We specifically count in a “pure” env. 
      This is not a mistake. There is no mutual recursion, 
      each declaration in and cannot use the others.*)
      let* _, ty_expr = infer_expr env expr in
      let* extended_env, extend_name_list = extend_env_with_pattern extended_env name_list pat ty_expr in
      return (extended_env, extend_name_list)
    ) (return (env, name_list)) cases
  
  | Ast.DRecursive (case, cases) ->
    let cases = case :: cases in

    (* Check several bounds *)
    let patterns = List.map fst cases in
    let* _ = UniquePatternVarsChecker.check_unique_vars patterns in

    let extend_env_with_pattern env name_list pat ty =
      match pat with
      | Ast.PVar (Id v) ->
        let generalized_ty = Generalize.generalize env ty in
        let new_names_list = update_name_list v name_list in
        return ((TypeEnv.extend env v generalized_ty), new_names_list)
      | _ ->
        fail InvalidRecursionLeftHand
    in

    let add_temporary_vars env cases =
      List.fold_left (fun acc (pat, _) ->
        let* env = acc in
        match pat with
        | Ast.PVar (Id name) ->
          let* fv = fresh_var in
          return (TypeEnv.extend env name (Schema.Schema (TypeVarSet.empty, fv)))
        | _ -> fail InvalidRecursionLeftHand
      ) (return env) cases
    in

    let process_cases env cases =
      List.fold_left (fun acc (pat, expr) ->
        let* env, name_list = acc in
        let* sub, ty_expr = infer_expr env expr in
        let env' = TypeEnv.apply env sub in
        let* extended_env, extend_name_list = extend_env_with_pattern env' name_list pat ty_expr in
        return (extended_env, extend_name_list)
      ) (return (env, name_list)) cases
    in

    let* env_with_vars = add_temporary_vars env cases in
    process_cases env_with_vars cases
;;

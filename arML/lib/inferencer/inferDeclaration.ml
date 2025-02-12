(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateResultMonad
open Common.StateResultMonad.Syntax
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
       | Ast.PListConstructor (l, r), TypeTree.TList t ->
         let* env, name_list = extend_env_with_pattern env name_list l t in
         extend_env_with_pattern env name_list r (TypeTree.TList t)
       | Ast.PAny, _
       | Ast.PNill, TypeTree.TList _ -> return (env, name_list)
       | Ast.PConst _ as pat, ct -> 
         let* t, _ = infer_pattern env pat in
         if t = ct then return (env, name_list) else fail @@ Unification_failed (t, ct)
       | pat, ty -> 
         let* typ, _ = infer_pattern env pat in
         fail @@ Unification_failed (typ, ty))
    in

    (* Check several bounds *)
    let patterns = List.map fst cases in
    let* _ = UniquePatternVarsChecker.check_unique_vars patterns in

    List.fold_left (fun acc (pat, expr) ->
        let expr = 
          (match expr with
           | Ast.ETyped(EFun (ps, body), typ) -> Ast.EFun (ps, Ast.ETyped(body, typ))
           | _ -> expr)
        in
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
          let* env, vars = acc in
          match pat with
          | Ast.PVar (Id name) ->
            let* fv = fresh_var in
            (* let* env', _ = extend_env_with_pattern env name_list pat fv in *)
            let env' = TypeEnv.extend env name (Schema.Schema (TypeVarSet.empty, fv)) in
            return (env', (name, fv) :: vars)
          | _ -> fail InvalidRecursionLeftHand
        ) (return (env, [])) cases
    in

    let process_cases env cases temp_vars =
      List.fold_left (fun acc (pat, expr) ->
          let expr = 
            (match expr with
             | Ast.ETyped(EFun (ps, body), typ) -> Ast.EFun (ps, Ast.ETyped(body, typ))
             | _ -> expr)
          in
          let* extract_var_name =
            (match pat with
             | Ast.PVar (Id name) -> return name
             | _ -> fail InvalidRecursionLeftHand)
          in
          let* env, name_list = acc in
          let* sub, ty_expr = infer_expr env expr in
          let env' = TypeEnv.apply env sub in
          let* extended_env, extend_name_list = extend_env_with_pattern env' name_list pat ty_expr in
          let* sub_update =
            match List.assoc_opt extract_var_name temp_vars with
            | Some temp_ty -> Substitution.unify temp_ty ty_expr
            | None -> return Substitution.empty
          in
          let* sub_final = Substitution.compose sub sub_update in
          let extended_env' = TypeEnv.apply extended_env sub_final in
          return (extended_env', extend_name_list)
        ) (return (env, name_list)) cases
    in

    let* env_with_vars, temp_vars = add_temporary_vars env cases in
    process_cases env_with_vars cases temp_vars
;;

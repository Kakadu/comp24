(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateResultMonad
open Common.StateResultMonad.Syntax
open TypeTree
open TypeErrors
open TypeUtils
open InferBasic
open InferPattern
open Ast.AbstractSyntaxTree

let infer_expr =
  let rec helper env = function
    | EConstant c -> return @@ (Substitution.empty, InferBasic.infer_const c)
    | EIdentifier (Id name) -> InferBasic.infer_id env name
    | EFun ((first_pattern, param_patterns), expr) ->
      infer_fun env (first_pattern :: param_patterns) expr
    | EApplication (func_expr, arg, args_exprs) ->
      infer_application env func_expr (arg :: args_exprs)
    | EIfThenElse (c, b1, b2) -> infer_if_then_else env c b1 b2
    | ETuple (first_pattern, second_pattern, pattern_list) ->
      infer_tuple env (first_pattern :: second_pattern :: pattern_list)
    | EListConstructor (head, tail) -> infer_list_constructor env head tail
    | EEmptyList ->
      let* fv = fresh_var in
      return (Substitution.empty, TList fv)
    | ELetIn (case, cases, expr) -> infer_let_in env (case :: cases) expr
    | ERecLetIn (case, cases, expr) -> infer_rec_let_in env (case :: cases) expr
    | EMatchWith (expr, case, cases) -> infer_match_with env expr (case :: cases)
    | EFunction (case, cases) -> infer_function env (case :: cases)
    | ETyped (expr, typ) -> infer_typed_expression env expr typ
  and infer_fun env ps e =
    let* ps_ty, ps_env =
      RList.fold_left
        ps
        ~init:(return ([], env))
        ~f:(fun (acc_ty, acc_env) p ->
            let* p_ty, p_env = infer_pattern acc_env p in
            return (p_ty :: acc_ty, p_env))
    in
    let* e_sub, e_ty = helper ps_env e in
    let fun_ty = List.fold_right (fun ty acc -> ty @-> acc) (List.rev ps_ty) e_ty in
    let result_ty = Substitution.apply e_sub fun_ty in
    return (e_sub, result_ty)
  and infer_application env func_expr args_exprs =
    let* sub', func_ty = helper env func_expr in
    List.fold_left
      (fun acc arg_expr ->
         let* acc_sub, acc_func_ty = acc in
         let env' = TypeEnv.apply env acc_sub in
         let* arg_sub, arg_ty = helper env' arg_expr in
         let* fv = fresh_var in
         let ty1 = Substitution.apply arg_sub acc_func_ty in
         let ty2 = arg_ty @-> fv in
         let* unify_sub = Substitution.unify ty1 ty2 in
         let* combined_sub = Substitution.compose_all [ acc_sub; arg_sub; unify_sub ] in
         let result_ty = Substitution.apply combined_sub fv in
         return (combined_sub, result_ty))
      (return (sub', func_ty))
      args_exprs
  and infer_if_then_else env c b1 b2 =
    let* sub', c_ty = helper env c in
    let* sub'', b1_ty = helper env b1 in
    let* sub''', b2_ty =
      match b2 with
      | Some expr -> helper env expr
      | None ->
        let* fv = fresh_var in
        return (Substitution.empty, fv)
    in
    let* sub'''' = Substitution.unify c_ty (TGround GTBool) in
    let* sub''''' = Substitution.unify b1_ty b2_ty in
    let* final_sun =
      Substitution.compose_all [ sub'; sub''; sub'''; sub''''; sub''''' ]
    in
    let final_ty = Substitution.apply final_sun b2_ty in
    return (final_sun, final_ty)
  and infer_tuple env es =
    let rec infer_tuple acc = function
      | [] -> return acc
      | hd :: tl ->
        let* sub', ty' = helper env hd in
        let acc_sub, acc_ty = acc in
        let* sub'' = Substitution.compose sub' acc_sub in
        let new_acc = sub'', ty' :: acc_ty in
        infer_tuple new_acc tl
    in
    let* sub, ty = infer_tuple (Substitution.empty, []) es in
    let ty_list = List.rev_map (Substitution.apply sub) ty in
    let ty = TTuple ty_list in
    return (sub, ty)
  and infer_list_constructor env l r =
    let* sub', l_ty = helper env l in
    let env' = TypeEnv.apply env sub' in
    let* sub'', r_ty = helper env' r in
    let* fv = fresh_var in
    let* sub''' = Substitution.unify (TList l_ty) fv in
    let* sub'''' = Substitution.unify r_ty fv in
    let* final_sub = Substitution.compose_all [ sub'; sub''; sub'''; sub'''' ] in
    let ty = Substitution.apply final_sub fv in
    return (final_sub, ty)
  and infer_let_in env cases expr =
    let rec extend_env_with_pattern env subs = function
      | PVar (Id v), ty ->
        let generalized_ty = Generalize.generalize env ty in
        return @@ (TypeEnv.extend env v generalized_ty, subs)
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
        extend_env_with_pattern (TypeEnv.apply env sub) (sub :: subs) (pat, new_ty)
      | PTuple (p1, p2, ps), TypeTree.TTuple (t1 :: t2 :: rest)
        when List.length rest = List.length ps ->
        let* env, subs = extend_env_with_pattern env subs (p1, t1) in
        let* env, subs = extend_env_with_pattern env subs (p2, t2) in
        List.fold_left2
          (fun acc pat ty ->
             let* env, subs' = acc in
             extend_env_with_pattern env subs' (pat, ty))
          (return @@ (env, subs))
          ps
          rest
      | (PListConstructor _ as pat), (TypeTree.TVar _ as ty) ->
        let* fv = fresh_var in
        let new_ty = TList fv in
        let* sub = Substitution.unify ty new_ty in
        extend_env_with_pattern (TypeEnv.apply env sub) (sub :: subs) (pat, new_ty)
      | PListConstructor (l, r), TypeTree.TList t ->
        let* env, subs = extend_env_with_pattern env subs (l, t) in
        extend_env_with_pattern env subs (r, TypeTree.TList t)
      | (PConst _ as pat), pty ->
        let* pt, _ = infer_pattern env pat in
        let* sub = Substitution.unify pt pty in
        return (env, sub :: subs)
      | PNill, TypeTree.TList _ | PAny, _ -> return (env, subs)
      | pat, ty ->
        let* typ, _ = infer_pattern env pat in
        fail @@ Unification_failed (typ, ty)
    in
    (* Check several bounds *)
    let all_patterns = List.map fst cases in
    let* _ = UniquePatternVarsChecker.check_unique_vars all_patterns in
    let* cases_env, cases_sub =
      List.fold_left
        (fun acc (pat, expr) ->
           let expr =
             match expr with
             | ETyped (EFun (ps, body), typ) -> EFun (ps, ETyped (body, typ))
             | _ -> expr
           in
           let* env, sub = acc in
           let* sub_expr, ty_expr = helper env expr in
           let* env', sub' = extend_env_with_pattern env [] (pat, ty_expr) in
           let* sub'' = Substitution.compose_all sub' in
           let* sub''' = Substitution.compose sub sub_expr in
           let* sub_final = Substitution.compose sub'' sub''' in
           return (env', sub_final))
        (return (env, Substitution.empty))
        cases
    in
    let* expr_sub, expr_ty = helper cases_env expr in
    let* final_sub = Substitution.compose expr_sub cases_sub in
    let final_ty = Substitution.apply final_sub expr_ty in
    return (final_sub, final_ty)
  and infer_rec_let_in env cases expr =
    let extend_env_with_pattern env = function
      | PVar (Id v), ty ->
        let generalized_ty = Generalize.generalize env ty in
        return (TypeEnv.extend env v generalized_ty)
      | _ -> fail InvalidRecursionLeftHand
    in
    let all_patterns = List.map fst cases in
    let* _ = UniquePatternVarsChecker.check_unique_vars all_patterns in
    let add_temporary_vars env cases =
      List.fold_left
        (fun acc (pat, _) ->
           let* env', vars = acc in
           match pat with
           | PVar (Id name) ->
             let* fv = fresh_var in
             let env'' = TypeEnv.extend env' name (Schema.Schema (TypeVarSet.empty, fv)) in
             return (env'', (name, fv) :: vars)
           | _ -> fail InvalidRecursionLeftHand)
        (return (env, []))
        cases
    in
    let process_cases env cases temp_vars =
      List.fold_left
        (fun acc (pat, expr) ->
           let expr =
             match expr with
             | ETyped (EFun (ps, body), typ) -> EFun (ps, ETyped (body, typ))
             | _ -> expr
           in
           let* extracted_var_name =
             match pat with
             | PVar (Id name) -> return name
             | _ -> fail InvalidRecursionLeftHand
           in
           let* env, sub = acc in
           let* sub_expr, ty_expr = helper env expr in
           let env' = TypeEnv.apply env sub_expr in
           let* env'' = extend_env_with_pattern env' (pat, ty_expr) in
           let* sub_update =
             match List.assoc_opt extracted_var_name temp_vars with
             | Some temp_ty -> Substitution.unify temp_ty ty_expr
             | None -> return Substitution.empty
           in
           let* sub_final = Substitution.compose sub sub_update in
           return (env'', sub_final))
        (return (env, Substitution.empty))
        cases
    in
    let* env_with_vars, temp_vars = add_temporary_vars env cases in
    let* final_env, final_sub = process_cases env_with_vars cases temp_vars in
    let* sub_expr, ty_expr = helper final_env expr in
    let* sub_final = Substitution.compose final_sub sub_expr in
    return (sub_final, ty_expr)
  and infer_cases env (init_sub, init_expr) cs =
    let* fv = fresh_var in
    let f acc case =
      let acc_sub, acc_ty = acc in
      let p, e = case in
      let* p_ty, p_env = infer_pattern env p in
      let* sub' = Substitution.unify init_expr p_ty in
      let env' = TypeEnv.apply p_env sub' in
      let* expr_sub, expr_ty = helper env' e in
      let* sub'' = Substitution.unify expr_ty acc_ty in
      let* sub = Substitution.compose_all [ acc_sub; expr_sub; sub'; sub'' ] in
      let ty = Substitution.apply sub acc_ty in
      return (sub, ty)
    in
    RList.fold_left cs ~init:(return (init_sub, fv)) ~f
  and infer_match_with env expr cs =
    let* sub, ty = helper env expr in
    let env' = TypeEnv.apply env sub in
    infer_cases env' (sub, ty) cs
  and infer_function env cs =
    let* fv = fresh_var in
    let* sub, ty = infer_cases env (Substitution.empty, fv) cs in
    return (sub, Substitution.apply sub (fv @-> ty))
  and infer_typed_expression env e e_ty =
    let* sub', ty = helper env e in
    let* expected_ty = get_type_by_defenition e_ty in
    let* sub'' = Substitution.unify ty expected_ty in
    let* final_sub = Substitution.compose sub' sub'' in
    return (final_sub, Substitution.apply final_sub ty)
  in
  helper
;;

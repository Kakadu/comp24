(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open StateResultMonad
open StateResultMonad.Syntax
open TypeTree
open TypeErrors
open TypeUtils
open InferBasic
open InferPattern

let infer_expr =

  let rec helper env = function
  | Ast.EConstant c -> return @@ (Substitution.empty, infer_const c)
  | Ast.EIdentifier (Id name) -> infer_id env name
  | Ast.EIfThenElse (cond, branch1, branch2) -> infer_if_then_else env cond branch1 branch2
  | Ast.EFun ((first_pattern, param_patterns), expr) -> infer_fun env (first_pattern :: param_patterns) expr
  | Ast.EApplication (func_expr, args_exprs) -> infer_application env func_expr args_exprs
  | Ast.ETuple (first_pattern, second_pattern, pattern_list) -> infer_tuple env (first_pattern :: second_pattern :: pattern_list)
  | Ast.EListConstructor (head, tail) -> infer_list_constructor env head tail
  | Ast.EEmptyList -> fresh_var >>= fun fv -> return (Substitution.empty, TList fv)
  | Ast.ELetIn (case, cases, expr) -> infer_let_in env (case :: cases) expr
  | Ast.ERecLetIn (case, cases, expr) -> infer_rec_let_in env (case :: cases) expr
  | Ast.EMatchWith (expr, case, cases) -> infer_match_with env expr (case :: cases)
  | Ast.EFunction (case, cases) -> infer_function env (case :: cases)
  | Ast.ETyped (expr, typ) -> infer_typed_expression env expr typ

  and infer_if_then_else env cond branch1 branch2 =
    let* sub1, ty1 = helper env cond in
    let* sub2, ty2 = helper env branch1 in
    let* sub3, ty3 = match branch2 with
    | Some expr -> helper env expr
    | None ->
      let* fv = fresh_var in 
      return (Substitution.empty, fv)
    in
    let* sub4 = Substitution.unify ty1 (TGround GTBool) in
    let* sub5 = Substitution.unify ty2 ty3 in
    let* sub = Substitution.compose_all [ sub1; sub2; sub3; sub4; sub5 ] in
    let ty = Substitution.apply sub ty3 in
    return (sub, ty)

  and infer_fun env patterns expr =
    let* patterns_ty, pattern_env =
      RList.fold_left patterns
        ~init:(return ([], env))
        ~f:(fun (acc_ty, acc_env) pattern ->
          let* pattern_ty, pattern_env = infer_pattern acc_env pattern in
          return (pattern_ty :: acc_ty, pattern_env))
    in
    let* expr_sub, expr_ty = helper pattern_env expr in
    let patterns_ty =
      List.fold_right
        (fun ty acc -> ty @-> acc)
        (List.rev patterns_ty)
        expr_ty
    in
    let result_ty = Substitution.apply expr_sub patterns_ty in
    return (expr_sub, result_ty)
  
  and infer_application env func_expr args_exprs =
    let* sub1, func_ty = helper env func_expr in
    let result =
      List.fold_left
        (fun acc args_exprs ->
            let* acc_sub, acc_func_ty = acc in
            let env' = TypeEnv.apply env acc_sub in
            let* sub_arg, arg_ty = helper env' args_exprs in
            let* result_ty = fresh_var in
            let ty1 = Substitution.apply sub_arg acc_func_ty in
            let ty2 = arg_ty @-> result_ty in
            let* sub_unify = Substitution.unify ty1 ty2 in
            let* sub_combined = Substitution.compose_all [ acc_sub; sub_arg; sub_unify ] in
            let updated_func_ty = Substitution.apply sub_combined result_ty in
            return (sub_combined, updated_func_ty))
        (return (sub1, func_ty))
        args_exprs
    in
    result
  
  and infer_tuple env expr_list =
    let rec infer_tuple acc = function
    | [] -> return acc
    | hd :: tl ->
      let* sub1, ty1 = helper env hd in
      let acc_sub, acc_ty = acc in
      let* sub2 = Substitution.compose sub1 acc_sub in
      let new_acc = sub2, ty1 :: acc_ty in
      infer_tuple new_acc tl
    in
    let acc = Substitution.empty, [] in
    let* sub, ty = infer_tuple acc expr_list in
    let ty_list = List.rev_map (Substitution.apply sub) ty in
    let ty = TTuple ty_list in
    return (sub, ty)
  
  and infer_list_constructor env l r =
    let* sub1, ty1 = helper env l in
    let env' = TypeEnv.apply env sub1 in
    let* sub2, ty2 = helper env' r in
    let* fv = fresh_var in
    let* sub3 = Substitution.unify (TList ty1) fv in
    let* sub4 = Substitution.unify ty2 fv in
    let* sub = Substitution.compose_all [ sub1; sub2; sub3; sub4 ] in
    let ty = Substitution.apply sub fv in
    return (sub, ty)

  and infer_let_in env cases expr =
    let rec extend_env_with_pattern env pat ty =
      match pat, ty with
      | Ast.PVar (Id v), ty ->
        let generalized_ty = Generalize.generalize env ty in
        return (TypeEnv.extend env v generalized_ty)
      | Ast.PTuple (p1, p2, ps), TypeTree.TTuple ts ->
        (match ts with
        | t1 :: t2 :: rest when List.length rest = List.length ps ->
          let* env = extend_env_with_pattern env p1 t1 in
          let* env = extend_env_with_pattern env p2 t2 in
          List.fold_left2 (fun acc pat ty ->
            let* env = acc in
            extend_env_with_pattern env pat ty
          ) (return env) ps rest
        | _ -> 
          let* typ, _ = infer_pattern env pat in
          fail @@ Unification_failed (typ, ty))
      | Ast.PAny, _ -> return env
      | Ast.PNill, TypeTree.TList _ -> return env
      | pat, ty -> 
        let* typ, _ = infer_pattern env pat in
        fail @@ Unification_failed (typ, ty)
    in

    (* Check several bounds *)
    let all_patterns = List.map fst cases in
    let* _ = UniquePatternVarsChecker.check_unique_vars all_patterns in
  
    let* final_env, final_sub = 
      List.fold_left (fun acc (pat, expr) ->
        let* env, sub = acc in
        let* sub_expr, ty_expr = helper env expr in
        let* env = extend_env_with_pattern env pat ty_expr in
        let* sub_final = Substitution.compose sub sub_expr in
        return (env, sub_final)
      ) (return (env, Substitution.empty)) cases
    in
  
    let* sub2, ty2 = helper final_env expr in
    let* sub_final = Substitution.compose final_sub sub2 in
    let final_ty = Substitution.apply sub_final ty2 in
    return (sub_final, final_ty)
  
    and infer_rec_let_in env cases expr =

      let extend_env_with_pattern env pat ty =
        match pat with
        | Ast.PVar (Id v) ->
          let generalized_ty = Generalize.generalize env ty in
          return (TypeEnv.extend env v generalized_ty)
        | _ ->
          fail InvalidRecursionLeftHand
      in
  
      let all_patterns = List.map fst cases in
      let* _ = UniquePatternVarsChecker.check_unique_vars all_patterns in
    
      let add_temporary_vars env cases =
        List.fold_left (fun acc (pat, _) ->
          let* env, vars = acc in
          match pat with
          | Ast.PVar (Id name) ->
            let* fv = fresh_var in
            let env' = TypeEnv.extend env name (Schema.Schema (TypeVarSet.empty, fv)) in
            return (env', (name, fv) :: vars)
          | _ -> fail InvalidRecursionLeftHand
        ) (return (env, [])) cases
          in
    
      let process_cases env cases temp_vars =
        List.fold_left (fun acc (pat, expr) ->
          
          let* extract_var_name =
            (match pat with
            | Ast.PVar (Id name) -> return name
            | _ -> fail InvalidRecursionLeftHand) in
          let* env, sub = acc in
          let* sub_expr, ty_expr = helper env expr in
          let env' = TypeEnv.apply env sub_expr in
          let* env'' = extend_env_with_pattern env' pat ty_expr in
          let* sub_final = Substitution.compose sub sub_expr in

          let* sub_final = 
            match List.assoc_opt extract_var_name temp_vars with
            | Some temp_ty -> 
              let* sub = Substitution.unify temp_ty ty_expr in
              Substitution.compose sub_final sub
            | None -> return sub_final
          in
          
          return (env'', sub_final)
        ) (return (env, Substitution.empty)) cases
      in
    
      let* env_with_vars, temp_vars = add_temporary_vars env cases in
      let* final_env, final_sub = process_cases env_with_vars cases temp_vars in
      let* sub2, ty2 = helper final_env expr in
      let* sub_final = Substitution.compose final_sub sub2 in
      return (sub_final, ty2)
  
  and infer_cases env (init_sub, init_expr) cases =
    let* fv = fresh_var in
    let f acc case =
      let acc_sub, acc_ty = acc in
      let pat, expr = case in
      let* pat_ty, pat_env = infer_pattern env pat in
      let* sub2 = Substitution.unify init_expr pat_ty in
      let env3 = TypeEnv.apply pat_env sub2 in
      let* expr_sub, expr_ty = helper env3 expr in
      let* sub3 = Substitution.unify expr_ty acc_ty in
      let* sub = Substitution.compose_all [ acc_sub; expr_sub; sub2; sub3 ] in
      let ty = Substitution.apply sub acc_ty in
      return (sub, ty)
    in
    RList.fold_left cases ~init:(return (init_sub, fv)) ~f
  
  and infer_match_with env expr cases =
    let* sub1, ty1 = helper env expr in
    let env2 = TypeEnv.apply env sub1 in
    infer_cases env2 (sub1, ty1) cases
  
  and infer_function env cases =
    let* fv = fresh_var in
    let* sub, expr_ty = infer_cases env (Substitution.empty, fv) cases in
    return (sub, Substitution.apply sub (fv @-> expr_ty))
  
  and infer_typed_expression env expr expr_typ =
    let* sub, ty = helper env expr in
    let* expr_ty = get_type_by_annotation expr_typ in
    let* sub2 = Substitution.unify ty expr_ty in
    let* final_sub = Substitution.compose sub sub2 in
    return (final_sub, Substitution.apply final_sub ty)
  in

  helper
;;

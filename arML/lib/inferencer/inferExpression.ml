(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open TypeTree
open StateResultMonad
open StateResultMonad.Syntax
open TypeErrors
open InferPattern
open TypeUtils
open CommonFunctions

let infer_expr =

  let rec helper env = function
  | Ast.EConstant c -> return @@ (Substitution.empty, infer_const c)
  | Ast.EIdentifier (Id name) -> infer_id env name
  | Ast.EIfThenElse (cond, branch1, branch2) -> infer_if_then_else env cond branch1 branch2
  | Ast.EFun ((first_pattern, param_patterns), expr) -> infer_fun env (first_pattern :: param_patterns) expr
  | Ast.EApplication (func_expr, args_exprs) -> infer_application env func_expr args_exprs
  | Ast.ETuple (first_pattern, second_pattern, pattern_list) -> infer_tuple env (first_pattern :: second_pattern :: pattern_list)
  | Ast.ELetIn ((pattern, expr1), expr2) -> infer_let_in env pattern expr1 expr2
  | Ast.ERecLetIn ((pattern, expr1), expr2) -> infer_rec_let_in env pattern expr1 expr2
  | Ast.EMatchWith (expr, case, cases) -> infer_match_with env expr (case :: cases)
  | Ast.EFunction (case, cases) -> infer_function env (case :: cases)
  | _ -> fail Occurs_check (* !!! *)

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
  
  and infer_let_in env pattern expr1 expr2 =
    let* _ = UniquePatternVarsChecker.check_unique_vars pattern in
    (* Check several bounds *)
    let* sub1, ty1 = helper env expr1 in

    let env_after_expr1 = TypeEnv.apply env sub1 in

    let rec extend_env_with_pattern env pat ty =
      (match pat, ty with
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
        | _ -> fail Occurs_check) (* !!! *)
      | Ast.PAny, _ -> return env
      | Ast.PNill, TypeTree.TList _ -> return env
      | _ -> fail Occurs_check) (* !!! *)
    in

    let* extended_env = extend_env_with_pattern env_after_expr1 pattern ty1 in

    let* sub2, ty2 = helper extended_env expr2 in

    let* sub_final = Substitution.compose sub1 sub2 in
    let final_ty = Substitution.apply sub_final ty2 in
    return (sub_final, final_ty)  
  
  and infer_rec_let_in env pattern expr1 expr2 =
    match pattern with
    | Ast.PVar (Id name) ->
      let* fv = fresh_var in
      let env2 = TypeEnv.extend env name (Schema.Schema (TypeVarSet.empty, fv)) in
      let* sub1, ty1 = helper env2 expr1 in
      let* sub2 = Substitution.unify ty1 fv in
      let* sub3 = Substitution.compose sub1 sub2 in
      let ty3 = Substitution.apply sub3 fv in
      let env2 = TypeEnv.apply env sub3 in
      let schema = Generalize.generalize env ty3 in
      let env3 = TypeEnv.extend env2 name schema in
      let* sub4, ty4 = helper env3 expr2 in
      let* sub5 = Substitution.compose sub3 sub4 in
      return (sub5, ty4)
    | _ -> fail InvalidRecursionLeftHand
  
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

  in

  helper
;;

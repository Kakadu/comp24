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

  in

  helper
;;

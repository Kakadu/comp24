(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open S
open S.SMonad
open Containers

let checked_fresh_var old_var env =
  let is_bad_name =
    String.starts_with ~prefix:"pat" old_var
    || VarSSet.find_opt old_var Std.extended_std_var_set != None
  in
  match VarSSet.find_opt old_var env, is_bad_name with
  | Some _, _ | _, true ->
    let rec helper env =
      let* new_var = fresh_var in
      match VarSSet.find_opt new_var env with
      | None -> return (new_var, VarSSet.add new_var env)
      | Some _ -> helper env
    in
    helper env
  | _ -> return (old_var, VarSSet.add old_var env)
;;

let update_pattern_vars varMap env pat =
  let rec helper varMap env = function
    | Ast.PAny -> return (varMap, env, Ast.PAny)
    | Ast.PConst _ as const -> return (varMap, env, const)
    | Ast.PVar (Ast.Id id) ->
      let* new_id, updated_env = checked_fresh_var id env in
      let updated_varMap = VarSMap.add id new_id varMap in
      return (updated_varMap, updated_env, Ast.PVar (Ast.Id new_id))
    | Ast.PTuple pat_lst ->
      let* updated_varMap, updated_env, rev_new_pat_lst =
        List.fold_left
          (fun acc pat ->
            let* curr_varMap, curr_env, curr_pat_lst = acc in
            let* updated_varMap, updated_env, updated_pat =
              helper curr_varMap curr_env pat
            in
            return (updated_varMap, updated_env, updated_pat :: curr_pat_lst))
          (return (varMap, env, []))
          pat_lst
      in
      return (updated_varMap, updated_env, Ast.PTuple (List.rev rev_new_pat_lst))
    | Ast.PCons (pat1, pat2) ->
      let* updated_varMap, updated_env, updated_pat1 = helper varMap env pat1 in
      let* updated_varMap, updated_env, updated_pat2 =
        helper updated_varMap updated_env pat2
      in
      return (updated_varMap, updated_env, Ast.PCons (updated_pat1, updated_pat2))
    | Ast.PType (pat, _) -> helper varMap env pat
  in
  helper varMap env pat
;;

let rec convert_value_binding_expr varMap env value_binding =
  let pat, expr = value_binding in
  let* expr, env = convert_expr varMap env expr in
  return ((pat, expr), env)

and convert_expr varMap env expr =
  let rec helper varMap env = function
    | Ast.EConst _ as contant -> return (contant, env)
    | Ast.EVar (Ast.Id var_name) as var ->
      (match VarSMap.find_opt var_name varMap with
       | Some x -> return (Ast.EVar (Ast.Id x), env)
       | None -> return (var, env))
    | Ast.ETuple expr_lst ->
      let* rev_expr_lst, env =
        List.fold_left
          (fun acc expr ->
            let* acc, env = acc in
            let* expr, env = helper varMap env expr in
            return (expr :: acc, env))
          (return ([], env))
          expr_lst
      in
      return (Ast.ETuple (List.rev rev_expr_lst), env)
    | Ast.EFun (pat_lst, expr) ->
      let* rev_new_pat_lst, updated_varMap, env =
        List.fold_left
          (fun acc pat ->
            let* curr_pat_lst, curr_varMap, curr_env = acc in
            let* updated_varMap, updated_env, new_pattern =
              update_pattern_vars curr_varMap curr_env pat
            in
            return (new_pattern :: curr_pat_lst, updated_varMap, updated_env))
          (return ([], varMap, env))
          pat_lst
      in
      let* updated_expr, env = helper updated_varMap env expr in
      return (Ast.EFun (List.rev rev_new_pat_lst, updated_expr), env)
    | Ast.ELet (Ast.Nonrecursive, value_binding, expr2) ->
      let old_pattern, expr1 = value_binding in
      let* updated_varMap, updated_env, new_pattern =
        update_pattern_vars varMap env old_pattern
      in
      let new_value_binding = new_pattern, expr1 in
      let* new_value_binding, updated_env =
        convert_value_binding_expr varMap updated_env new_value_binding
      in
      let* new_expr2, updated_env = helper updated_varMap updated_env expr2 in
      return (Ast.ELet (Ast.Nonrecursive, new_value_binding, new_expr2), updated_env)
    | Ast.ELet (Ast.Recursive, value_binding, expr2) ->
      let old_pattern, expr1 = value_binding in
      let* updated_varMap, updated_env, new_pattern =
        update_pattern_vars varMap env old_pattern
      in
      let new_value_binding = new_pattern, expr1 in
      let* new_value_binding, updated_env =
        convert_value_binding_expr updated_varMap updated_env new_value_binding
      in
      let* new_expr2, updated_env = helper updated_varMap updated_env expr2 in
      return (Ast.ELet (Ast.Recursive, new_value_binding, new_expr2), updated_env)
    | Ast.EApp (expr1, expr2) ->
      let* new_expr1, env = helper varMap env expr1 in
      let* new_expr2, env = helper varMap env expr2 in
      return (Ast.EApp (new_expr1, new_expr2), env)
    | Ast.EMatch (expr, case_lst) ->
      let* new_expr, updated_env = helper varMap env expr in
      let* rev_new_case_lst, updated_env =
        List.fold_left
          (fun acc case ->
            let* curr_new_case_lst, curr_env = acc in
            let old_pattern, old_expr = case in
            let* updated_varMap, updated_env, new_pattern =
              update_pattern_vars varMap curr_env old_pattern
            in
            let* new_expr, updated_env = helper updated_varMap updated_env old_expr in
            return ((new_pattern, new_expr) :: curr_new_case_lst, updated_env))
          (return ([], updated_env))
          case_lst
      in
      return (Ast.EMatch (new_expr, List.rev rev_new_case_lst), updated_env)
    | Ast.EIf (expr1, expr2, expr3) ->
      let* new_expr1, env = helper varMap env expr1 in
      let* new_expr2, env = helper varMap env expr2 in
      let* new_expr3, env = helper varMap env expr3 in
      return (Ast.EIf (new_expr1, new_expr2, new_expr3), env)
    | Ast.ECons (expr1, expr2) ->
      let* new_expr1, env = helper varMap env expr1 in
      let* new_expr2, env = helper varMap env expr2 in
      return (Ast.ECons (new_expr1, new_expr2), env)
    | Ast.EType (expr, _) -> helper varMap env expr
  in
  helper varMap env expr
;;

let update_value_binding_lst_vars varMap env value_binding_lst =
  List.fold_left
    (fun acc value_binding ->
      let* value_bindings_with_new_vars, curr_varMap, env = acc in
      let old_pattern, expr = value_binding in
      let* updated_varMap, updated_env, new_pattern =
        update_pattern_vars curr_varMap env old_pattern
      in
      return
        ((new_pattern, expr) :: value_bindings_with_new_vars, updated_varMap, updated_env))
    (return ([], varMap, env))
    value_binding_lst
;;

let update_value_binding_lst_exprs varMap env value_binding_lst =
  List.fold_left
    (fun acc value_binding ->
      let* acc, env = acc in
      let* new_value_binding, env = convert_value_binding_expr varMap env value_binding in
      return (new_value_binding :: acc, env))
    (return ([], env))
    value_binding_lst
;;

let convert_structure structure =
  let helper varMap env = function
    | Ast.SILet (rec_flag, value_binding_lst) ->
      let* rev_value_bindings_with_new_vars, updated_varMap, env =
        update_value_binding_lst_vars varMap env value_binding_lst
      in
      let* new_value_bindings, env =
        match rec_flag with
        | Ast.Nonrecursive ->
          update_value_binding_lst_exprs varMap env rev_value_bindings_with_new_vars
        | Ast.Recursive ->
          update_value_binding_lst_exprs
            updated_varMap
            env
            rev_value_bindings_with_new_vars
      in
      return (Ast.SILet (rec_flag, new_value_bindings), updated_varMap, env)
    | Ast.SIExpr expr ->
      let* expr, env = convert_expr varMap env expr in
      return (Ast.SIExpr expr, varMap, env)
  in
  let* _, env, rev_structure =
    List.fold_left
      (fun acc structure_item ->
        let* varMap, env, si_lst = acc in
        let* new_si, new_varMap, env = helper varMap env structure_item in
        return (new_varMap, env, new_si :: si_lst))
      (return (VarSMap.empty, VarSSet.empty, []))
      structure
  in
  return (List.rev rev_structure, env)
;;

let run_alpha_conversion (structure : Ast.structure) = run (convert_structure structure)

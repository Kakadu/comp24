(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open S
open S.SMonad
open Containers

let checked_fresh_var env =
  let rec helper env =
    let* new_var = fresh_var in
    match VarSSet.find_opt new_var env with
    | None -> return (new_var, VarSSet.add new_var env)
    | Some _ -> helper env
  in
  helper env
;;

let id_from_var_name var_name = Ast.Id var_name

let rec lift_lambdas_from_value_binding value_binding env =
  let ident, expr = value_binding in
  match expr with
  | Simple_ast.SEFun (fid_lst, fexpr) ->
    let* fobaml_lst, new_expr, env = lift_lambdas_from_expr fexpr env in
    return (fobaml_lst, (ident, Simple_ast.SEFun (fid_lst, new_expr)), env)
  | _ ->
    let* fobaml_lst, new_expr, env = lift_lambdas_from_expr expr env in
    return (fobaml_lst, (ident, new_expr), env)

and lift_lambdas_from_expr expr env =
  let rec helper curr_fobaml_lst env = function
    | Simple_ast.SEConst _ as const -> return (curr_fobaml_lst, const, env)
    | Simple_ast.SEVar _ as var -> return (curr_fobaml_lst, var, env)
    | Simple_ast.SETuple tup_lst ->
      let* new_fobaml_lst, rev_new_tup_lst, env =
        List.fold_left
          (fun acc tup_expr ->
            let* curr_fobaml_lst, new_tup_lst, env = acc in
            let* new_fobaml_lst, new_tup_expr, env =
              helper curr_fobaml_lst env tup_expr
            in
            return (new_fobaml_lst, new_tup_expr :: new_tup_lst, env))
          (return (curr_fobaml_lst, [], env))
          tup_lst
      in
      return (new_fobaml_lst, Simple_ast.SETuple (List.rev rev_new_tup_lst), env)
    | Simple_ast.SEFun (ident_lst, expr) ->
      let* new_fobaml_var_name, env = checked_fresh_var env in
      let new_fobaml_id = id_from_var_name new_fobaml_var_name in
      let* new_fobaml_lst, new_expr, env = helper curr_fobaml_lst env expr in
      let new_fobaml_value_binding =
        Simple_ast.SId new_fobaml_id, Simple_ast.SEFun (ident_lst, new_expr)
      in
      let new_fobaml =
        Simple_ast.SSILet (Ast.Nonrecursive, [ new_fobaml_value_binding ])
      in
      let new_fobaml_lst = List.append new_fobaml_lst [ new_fobaml ] in
      return (new_fobaml_lst, Simple_ast.SEVar new_fobaml_id, env)
    | Simple_ast.SELet (rec_flag, value_binding, expr) ->
      let* additional_fobaml_lst, new_value_binding, env =
        lift_lambdas_from_value_binding value_binding env
      in
      let new_fobaml_lst = List.append curr_fobaml_lst additional_fobaml_lst in
      let _, vexp = value_binding in
      (match vexp with
       | Simple_ast.SEFun _ ->
         let new_fobaml_lst =
           List.append
             new_fobaml_lst
             [ Simple_ast.SSILet (rec_flag, [ new_value_binding ]) ]
         in
         let* new_fobaml_lst, new_expr, env = helper new_fobaml_lst env expr in
         return (new_fobaml_lst, new_expr, env)
       | _ ->
         let* new_fobaml_lst, new_expr, env = helper new_fobaml_lst env expr in
         return
           (new_fobaml_lst, Simple_ast.SELet (rec_flag, new_value_binding, new_expr), env))
    | Simple_ast.SEApp (expr1, expr2) ->
      let* new_fobaml_lst, new_expr1, env = helper curr_fobaml_lst env expr1 in
      let* new_fobaml_lst, new_expr2, env = helper new_fobaml_lst env expr2 in
      return (new_fobaml_lst, Simple_ast.SEApp (new_expr1, new_expr2), env)
    | Simple_ast.SEIf (expr1, expr2, expr3) ->
      let* new_fobaml_lst, new_expr1, env = helper curr_fobaml_lst env expr1 in
      let* new_fobaml_lst, new_expr2, env = helper new_fobaml_lst env expr2 in
      let* new_fobaml_lst, new_expr3, env = helper new_fobaml_lst env expr3 in
      return (new_fobaml_lst, Simple_ast.SEIf (new_expr1, new_expr2, new_expr3), env)
    | Simple_ast.SECons (expr1, expr2) ->
      let* new_fobaml_lst, new_expr1, env = helper curr_fobaml_lst env expr1 in
      let* new_fobaml_lst, new_expr2, env = helper new_fobaml_lst env expr2 in
      return (new_fobaml_lst, Simple_ast.SECons (new_expr1, new_expr2), env)
  in
  helper [] env expr
;;

let lift_lambdas_from_value_binding_lst value_binding_lst env =
  let* new_fobaml_lst, rev_new_value_binding_lst, env =
    List.fold_left
      (fun acc value_binding ->
        let* curr_fobaml_lst, cur_new_value_binding_lst, env = acc in
        let* new_fobaml_lst, new_value_binding, env =
          lift_lambdas_from_value_binding value_binding env
        in
        return
          ( List.append curr_fobaml_lst new_fobaml_lst
          , new_value_binding :: cur_new_value_binding_lst
          , env ))
      (return ([], [], env))
      value_binding_lst
  in
  return (new_fobaml_lst, List.rev rev_new_value_binding_lst, env)
;;

let lift_lambdas_from_structure structure env =
  let helper env = function
    | Simple_ast.SSILet (rec_flag, value_binding_lst) ->
      let* new_fobaml_lst, new_value_binding_lst, env =
        lift_lambdas_from_value_binding_lst value_binding_lst env
      in
      return (new_fobaml_lst, Simple_ast.SSILet (rec_flag, new_value_binding_lst), env)
    | Simple_ast.SSIExpr expr ->
      let* new_fobaml_lst, new_expr, env = lift_lambdas_from_expr expr env in
      return (new_fobaml_lst, Simple_ast.SSIExpr new_expr, env)
  in
  let* rev_new_structure, env =
    List.fold_left
      (fun acc structure_item ->
        let* new_structure, env = acc in
        let* new_fobaml_lst, new_structure_item, env = helper env structure_item in
        return
          (List.append (new_structure_item :: List.rev new_fobaml_lst) new_structure, env))
      (return ([], env))
      structure
  in
  return (List.rev rev_new_structure, env)
;;

let run_lambda_lifting (structure : Simple_ast.sstructure) (env : VarSSet.t) =
  run (lift_lambdas_from_structure structure env)
;;

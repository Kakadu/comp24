(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open State_monad.StateMonad

let fresh_var_name = "foba"
let fresh_var = fresh >>| fun n -> fresh_var_name ^ string_of_int n
let id_from_var_name var_name = Ast.Id var_name

let rec lift_lambdas_from_value_binding value_binding =
  let ident, expr = value_binding in
  match expr with
  | Simple_ast.SEFun (fid_lst, fexpr) ->
    let* fobaml_lst, new_expr = lift_lambdas_from_expr fexpr in
    return (fobaml_lst, (ident, Simple_ast.SEFun (fid_lst, new_expr)))
  | _ ->
    let* fobaml_lst, new_expr = lift_lambdas_from_expr expr in
    return (fobaml_lst, (ident, new_expr))

and lift_lambdas_from_expr expr =
  let rec helper curr_fobaml_lst = function
    | Simple_ast.SEConst _ as const -> return (curr_fobaml_lst, const)
    | Simple_ast.SEVar _ as var -> return (curr_fobaml_lst, var)
    | Simple_ast.SETuple tup_lst ->
      let* new_fobaml_lst, rev_new_tup_lst =
        List.fold_left
          (fun acc tup_expr ->
            let* curr_fobaml_lst, new_tup_lst = acc in
            let* new_fobaml_lst, new_tup_expr = helper curr_fobaml_lst tup_expr in
            return (new_fobaml_lst, new_tup_expr :: new_tup_lst))
          (return (curr_fobaml_lst, []))
          tup_lst
      in
      return (new_fobaml_lst, Simple_ast.SETuple (List.rev rev_new_tup_lst))
    | Simple_ast.SEFun (ident_lst, expr) ->
      let* new_fobaml_var_name = fresh_var in
      let new_fobaml_id = id_from_var_name new_fobaml_var_name in
      let* new_fobaml_lst, new_expr = helper curr_fobaml_lst expr in
      let new_fobaml_value_binding =
        Simple_ast.SId new_fobaml_id, Simple_ast.SEFun (ident_lst, new_expr)
      in
      let new_fobaml =
        Simple_ast.SSILet (Ast.Nonrecursive, [ new_fobaml_value_binding ])
      in
      let new_fobaml_lst = List.append new_fobaml_lst [ new_fobaml ] in
      return (new_fobaml_lst, Simple_ast.SEVar new_fobaml_id)
    | Simple_ast.SELet (rec_flag, value_binding, expr) ->
      let* additional_fobaml_lst, new_value_binding =
        lift_lambdas_from_value_binding value_binding
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
         let* new_fobaml_lst, new_expr = helper new_fobaml_lst expr in
         return (new_fobaml_lst, new_expr)
       | _ ->
         let* new_fobaml_lst, new_expr = helper new_fobaml_lst expr in
         return (new_fobaml_lst, Simple_ast.SELet (rec_flag, new_value_binding, new_expr)))
    | Simple_ast.SEApp (expr1, expr2) ->
      let* new_fobaml_lst, new_expr1 = helper curr_fobaml_lst expr1 in
      let* new_fobaml_lst, new_expr2 = helper new_fobaml_lst expr2 in
      return (new_fobaml_lst, Simple_ast.SEApp (new_expr1, new_expr2))
    | Simple_ast.SEIf (expr1, expr2, expr3) ->
      let* new_fobaml_lst, new_expr1 = helper curr_fobaml_lst expr1 in
      let* new_fobaml_lst, new_expr2 = helper new_fobaml_lst expr2 in
      let* new_fobaml_lst, new_expr3 = helper new_fobaml_lst expr3 in
      return (new_fobaml_lst, Simple_ast.SEIf (new_expr1, new_expr2, new_expr3))
    | Simple_ast.SECons (expr1, expr2) ->
      let* new_fobaml_lst, new_expr1 = helper curr_fobaml_lst expr1 in
      let* new_fobaml_lst, new_expr2 = helper new_fobaml_lst expr2 in
      return (new_fobaml_lst, Simple_ast.SECons (new_expr1, new_expr2))
  in
  helper [] expr
;;

let lift_lambdas_from_value_binding_lst value_binding_lst =
  let* new_fobaml_lst, rev_new_value_binding_lst =
    List.fold_left
      (fun acc value_binding ->
        let* curr_fobaml_lst, cur_new_value_binding_lst = acc in
        let* new_fobaml_lst, new_value_binding =
          lift_lambdas_from_value_binding value_binding
        in
        return
          ( List.append curr_fobaml_lst new_fobaml_lst
          , new_value_binding :: cur_new_value_binding_lst ))
      (return ([], []))
      value_binding_lst
  in
  return (new_fobaml_lst, List.rev rev_new_value_binding_lst)
;;

let lift_lambdas_from_structure structure =
  let helper = function
    | Simple_ast.SSILet (rec_flag, value_binding_lst) ->
      let* new_fobaml_lst, new_value_binding_lst =
        lift_lambdas_from_value_binding_lst value_binding_lst
      in
      return (new_fobaml_lst, Simple_ast.SSILet (rec_flag, new_value_binding_lst))
    | Simple_ast.SSIExpr expr ->
      let* new_fobaml_lst, new_expr = lift_lambdas_from_expr expr in
      return (new_fobaml_lst, Simple_ast.SSIExpr new_expr)
  in
  let* rev_new_structure =
    List.fold_left
      (fun acc structure_item ->
        let* new_structure = acc in
        let* new_fobaml_lst, new_structure_item = helper structure_item in
        return (List.append (new_structure_item :: List.rev new_fobaml_lst) new_structure))
      (return [])
      structure
  in
  return (List.rev rev_new_structure)
;;

let run_lambda_lifting (structure : Simple_ast.sstructure) =
  run (lift_lambdas_from_structure structure)
;;

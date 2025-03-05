(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

let pattern_from_sident = function
  | Simple_ast.SSpecial SUnit -> Ast.PConst Ast.CUnit
  | Simple_ast.SId id -> Ast.PVar id
;;

let rec convert_value_binding svalue_binding =
  let ident, sexpr = svalue_binding in
  let expr = convert_expr sexpr in
  pattern_from_sident ident, expr

and convert_expr sexpr =
  let rec helper = function
    | Simple_ast.SEConst const -> Ast.EConst const
    | Simple_ast.SEVar var -> Ast.EVar var
    | Simple_ast.SETuple stup_lst ->
      let rev_tup_lst =
        List.fold_left
          (fun acc stup_expr ->
            let tup_expr = helper stup_expr in
            tup_expr :: acc)
          []
          stup_lst
      in
      Ast.ETuple (List.rev rev_tup_lst)
    | Simple_ast.SEFun (ident_lst, sexpr) ->
      let rev_pat_lst =
        List.fold_left (fun acc ident -> pattern_from_sident ident :: acc) [] ident_lst
      in
      let expr = helper sexpr in
      Ast.EFun (List.rev rev_pat_lst, expr)
    | Simple_ast.SELet (rec_flag, svalue_binding, sexpr) ->
      let value_binding = convert_value_binding svalue_binding in
      let expr = helper sexpr in
      Ast.ELet (rec_flag, value_binding, expr)
    | Simple_ast.SEApp (sexpr1, sexpr2) ->
      let expr1 = helper sexpr1 in
      let expr2 = helper sexpr2 in
      Ast.EApp (expr1, expr2)
    | Simple_ast.SEIf (sexpr1, sexpr2, sexpr3) ->
      let expr1 = helper sexpr1 in
      let expr2 = helper sexpr2 in
      let expr3 = helper sexpr3 in
      Ast.EIf (expr1, expr2, expr3)
    | Simple_ast.SECons (sexpr1, sexpr2) ->
      let expr1 = helper sexpr1 in
      let expr2 = helper sexpr2 in
      Ast.ECons (expr1, expr2)
  in
  helper sexpr
;;

let convert_value_binding_lst svalue_binding_lst =
  let new_value_binding_lst =
    List.fold_left
      (fun acc svalue_binding ->
        let value_binding = convert_value_binding svalue_binding in
        value_binding :: acc)
      []
      svalue_binding_lst
  in
  List.rev new_value_binding_lst
;;

let convert_structure_item simple_structure_item =
  let helper = function
    | Simple_ast.SSILet (rec_flag, svalue_binging_lst) ->
      let value_binding_lst = convert_value_binding_lst svalue_binging_lst in
      Ast.SILet (rec_flag, value_binding_lst)
    | Simple_ast.SSIExpr expr ->
      let expr = convert_expr expr in
      Ast.SIExpr expr
  in
  helper simple_structure_item
;;

let convert_structure simple_structure =
  let new_strcture =
    List.fold_left
      (fun acc simple_structure_item ->
        let structure_item = convert_structure_item simple_structure_item in
        structure_item :: acc)
      []
      simple_structure
  in
  List.rev new_strcture
;;

let convert (simple_structure : Simple_ast.sstructure) =
  convert_structure simple_structure
;;

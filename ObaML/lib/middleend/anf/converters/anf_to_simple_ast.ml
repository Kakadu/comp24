(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

let convert_ident = function
  | "()" -> Simple_ast.SSpecial SUnit
  | _ as var_name -> Simple_ast.SId (Ast.Id var_name)
;;

let convert_ident_lst ident_lst =
  let rev_ident_lst =
    List.fold_left (fun acc ident -> convert_ident ident :: acc) [] ident_lst
  in
  List.rev rev_ident_lst
;;

let rec convert_tuple tup_lst =
  let rev_tup_lst =
    List.fold_left (fun acc tup_expr -> convert_immexpr tup_expr :: acc) [] tup_lst
  in
  Simple_ast.SETuple (List.rev rev_tup_lst)

and convert_immexpr = function
  | Anf.ImmId id -> Simple_ast.SEVar (Ast.Id id)
  | Anf.ImmInt num -> Simple_ast.SEConst (Ast.CInt num)
  | Anf.ImmString str -> Simple_ast.SEConst (Ast.CString str)
  | Anf.ImmBool b -> Simple_ast.SEConst (Ast.CBool b)
  | Anf.ImmEmptyList -> Simple_ast.SEConst Ast.CEmptyList
  | Anf.ImmUnit -> Simple_ast.SEConst Ast.CUnit
  | Anf.ImmTuple tup_lst -> convert_tuple tup_lst
;;

let rec convert_cexpr = function
  | Anf.CImmExpr immexpr -> convert_immexpr immexpr
  | Anf.CApp (immexpr, args_lst) ->
    let expr1 = convert_immexpr immexpr in
    List.fold_left
      (fun curr_expr arg ->
        let converted_arg = convert_immexpr arg in
        Simple_ast.SEApp (curr_expr, converted_arg))
      expr1
      args_lst
  | Anf.CIf (immexpr, aexpr1, aexpr2) ->
    let expr1 = convert_immexpr immexpr in
    let expr2 = convert_aexpr aexpr1 in
    let expr3 = convert_aexpr aexpr2 in
    Simple_ast.SEIf (expr1, expr2, expr3)
  | Anf.CCons (immexpr1, immexpr2) ->
    let expr1 = convert_immexpr immexpr1 in
    let expr2 = convert_immexpr immexpr2 in
    Simple_ast.SECons (expr1, expr2)

and convert_aexpr = function
  | Anf.ALet (name, cexpr, aexpr) ->
    let ident = convert_ident name in
    let expr1 = convert_cexpr cexpr in
    let expr2 = convert_aexpr aexpr in
    Simple_ast.SELet (Ast.Nonrecursive, (ident, expr1), expr2)
  | Anf.ACExpr cexpr -> convert_cexpr cexpr
;;

let convert_value_binding value_binding =
  let name, args, aexpr = value_binding in
  let ident = convert_ident name in
  let expr = convert_aexpr aexpr in
  match args with
  | [] -> ident, expr
  | _ -> ident, Simple_ast.SEFun (convert_ident_lst args, expr)
;;

let convert_value_bindings value_bindings =
  let rev_converted_value_bindings =
    List.fold_left
      (fun acc value_binding -> convert_value_binding value_binding :: acc)
      []
      value_bindings
  in
  List.rev rev_converted_value_bindings
;;

let convert_tp tp =
  let rec_flag, value_bindings = tp in
  let converted_value_bindings = convert_value_bindings value_bindings in
  let converted_rec_flag =
    match rec_flag with
    | true -> Ast.Recursive
    | false -> Ast.Nonrecursive
  in
  Simple_ast.SSILet (converted_rec_flag, converted_value_bindings)
;;

let convert_program program =
  let rev_structure_items =
    List.fold_left (fun acc tp -> convert_tp tp :: acc) [] program
  in
  List.rev rev_structure_items
;;

let convert (program : Anf.program) = convert_program program

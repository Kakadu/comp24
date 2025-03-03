(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open RS
open RS.RSMonad
open RS.RSMonad.Syntax

let convert_ident ident =
  let helper = function
    | Simple_ast.SId (Ast.Id id) -> return id
    | Simple_ast.SSpecial SUnit -> return "()"
  in
  helper ident
;;

let const_to_immexpr const =
  let helper = function
    | Ast.CInt n -> Anf.ImmInt n
    | Ast.CString s -> Anf.ImmString s
    | Ast.CBool b -> Anf.ImmBool b
    | Ast.CEmptyList -> Anf.ImmEmptyList
    | Ast.CUnit -> Anf.ImmUnit
  in
  return (helper const)
;;

let rec convert_tup_lst_to_cexpr tup_lst =
  let helper tup_expr =
    let* fresh_var = fresh_var in
    let* additions, new_expr = expr_to_cexpr tup_expr in
    let* all_additions, fresh_var =
      match new_expr with
      | Anf.CImmExpr new_imm_expr -> return (additions, new_imm_expr)
      | _ -> return (List.append additions [ fresh_var, new_expr ], Anf.ImmId fresh_var)
    in
    return (fresh_var, all_additions)
  in
  let* additions, rev_new_tup_lst =
    List.fold_left
      (fun acc tup_expr ->
        let* curr_additions, curr_rev_tup_lst = acc in
        let* immexpr, new_additions = helper tup_expr in
        return (List.append curr_additions new_additions, immexpr :: curr_rev_tup_lst))
      (return ([], []))
      tup_lst
  in
  return (additions, Anf.CImmExpr (Anf.ImmTuple (List.rev rev_new_tup_lst)))

and convert_app_to_cexpr expr1 expr2 =
  let rec helper expr1 expr2 =
    match expr1 with
    | Simple_ast.SEApp (in_expr1, in_expr2) ->
      let* additions, app_var, args = helper in_expr1 in_expr2 in
      let* additions2, new_expr2 = expr_to_cexpr expr2 in
      let all_additions = List.append additions additions2 in
      let* all_additions, fresh =
        match new_expr2 with
        | Anf.CImmExpr new_imm_expr2 -> return (all_additions, new_imm_expr2)
        | _ ->
          let* fresh = fresh_var in
          let all_additions = List.append all_additions [ fresh, new_expr2 ] in
          return (all_additions, Anf.ImmId fresh)
      in
      return (all_additions, app_var, List.append args [ fresh ])
    | _ ->
      let* additions1, new_expr1 = expr_to_cexpr expr1 in
      let* all_additions, fresh1 =
        match new_expr1 with
        | Anf.CImmExpr new_imm_expr1 -> return (additions1, new_imm_expr1)
        | _ ->
          let* fresh1 = fresh_var in
          let all_additions = List.append additions1 [ fresh1, new_expr1 ] in
          return (all_additions, Anf.ImmId fresh1)
      in
      let* additions2, new_expr2 = expr_to_cexpr expr2 in
      let all_additions = List.append all_additions additions2 in
      let* all_additions, fresh2 =
        match new_expr2 with
        | Anf.CImmExpr new_imm_expr2 -> return (all_additions, new_imm_expr2)
        | _ ->
          let* fresh2 = fresh_var in
          let all_additions = List.append all_additions [ fresh2, new_expr2 ] in
          return (all_additions, Anf.ImmId fresh2)
      in
      return (all_additions, fresh1, [ fresh2 ])
  in
  let* all_additions, app_var, args = helper expr1 expr2 in
  return (all_additions, Anf.CApp (app_var, args))

and expr_to_cexpr expr =
  let rec helper = function
    | Simple_ast.SEConst const ->
      let* const = const_to_immexpr const in
      return ([], Anf.CImmExpr const)
    | Simple_ast.SEVar (Ast.Id var) -> return ([], Anf.CImmExpr (ImmId var))
    | Simple_ast.SETuple tup_lst -> convert_tup_lst_to_cexpr tup_lst
    | Simple_ast.SEFun _ -> fail "Unexpected fun"
    | Simple_ast.SELet (Ast.Nonrecursive, value_binding, expr2) ->
      let ident, expr1 = value_binding in
      let* ident_var = convert_ident ident in
      let* additions1, new_expr1 = helper expr1 in
      let all_additions = List.append additions1 [ ident_var, new_expr1 ] in
      let* additions2, new_expr2 = helper expr2 in
      let all_additions = List.append all_additions additions2 in
      return (all_additions, new_expr2)
    | Simple_ast.SELet (Ast.Recursive, _, _) -> fail "Unexpected recursion"
    | Simple_ast.SEApp (expr1, expr2) -> convert_app_to_cexpr expr1 expr2
    | Simple_ast.SEIf (expr1, expr2, expr3) ->
      let* additions, new_expr = helper expr1 in
      let* all_additions, fresh_var =
        match new_expr with
        | Anf.CImmExpr new_imm_expr -> return (additions, new_imm_expr)
        | _ ->
          let* fresh_var = fresh_var in
          let all_additions = List.append additions [ fresh_var, new_expr ] in
          return (all_additions, Anf.ImmId fresh_var)
      in
      let* new_expr2 = expr_to_aexpr expr2 in
      let* new_expr3 = expr_to_aexpr expr3 in
      return (all_additions, Anf.CIf (fresh_var, new_expr2, new_expr3))
    | Simple_ast.SECons (expr1, expr2) ->
      let* additions1, new_expr1 = helper expr1 in
      let* all_additions, fresh_var1 =
        match new_expr1 with
        | Anf.CImmExpr new_imm_expr1 -> return (additions1, new_imm_expr1)
        | _ ->
          let* fresh_var1 = fresh_var in
          let all_additions = List.append additions1 [ fresh_var1, new_expr1 ] in
          return (all_additions, Anf.ImmId fresh_var1)
      in
      let* additions2, new_expr2 = helper expr2 in
      let all_additions = List.append all_additions additions2 in
      let* all_additions, fresh_var2 =
        match new_expr2 with
        | Anf.CImmExpr new_imm_expr2 -> return (all_additions, new_imm_expr2)
        | _ ->
          let* fresh_var2 = fresh_var in
          let all_additions = List.append all_additions [ fresh_var2, new_expr2 ] in
          return (all_additions, Anf.ImmId fresh_var2)
      in
      return (all_additions, Anf.CCons (fresh_var1, fresh_var2))
  in
  helper expr

and expr_to_aexpr expr =
  let rec helper = function
    | Simple_ast.SELet (Ast.Nonrecursive, value_binding, expr2) ->
      let ident, expr1 = value_binding in
      let* new_ident = convert_ident ident in
      let* additional_bindings1, new_expr1 = expr_to_cexpr expr1 in
      let* aexpr2 = helper expr2 in
      let new_value_binding = Anf.ALet (new_ident, new_expr1, aexpr2) in
      let* new_value_binding_with_additions =
        List.fold_left
          (fun curr addition ->
            let* curr = curr in
            let add_ident, add_expr = addition in
            return (Anf.ALet (add_ident, add_expr, curr)))
          (return new_value_binding)
          (List.rev additional_bindings1)
      in
      return new_value_binding_with_additions
    | Simple_ast.SELet (Ast.Recursive, _, _) -> fail "Unexpected rec"
    | _ as expr ->
      let* additional_bindings, new_expr = expr_to_cexpr expr in
      List.fold_left
        (fun acc addition ->
          let* acc = acc in
          let add_ident, add_expr = addition in
          return (Anf.ALet (add_ident, add_expr, acc)))
        (return (Anf.ACExpr new_expr))
        (List.rev additional_bindings)
  in
  helper expr
;;

let convert_value_binding_lst value_binding_lst =
  let helper value_binding =
    let ident, expr = value_binding in
    let* new_ident = convert_ident ident in
    let* args_lst, new_expr =
      match expr with
      | Simple_ast.SEFun (id_lst, fexpr) ->
        let* rev_args =
          List.fold_left
            (fun acc id ->
              let* acc = acc in
              let* new_id = convert_ident id in
              return (new_id :: acc))
            (return [])
            id_lst
        in
        let* new_expr = expr_to_aexpr fexpr in
        return (List.rev rev_args, new_expr)
      | _ ->
        let* new_expr = expr_to_aexpr expr in
        return ([], new_expr)
    in
    return (new_ident, args_lst, new_expr)
  in
  let* rev_value_binding_lst =
    List.fold_left
      (fun acc value_binding ->
        let* acc = acc in
        let* new_value_binding = helper value_binding in
        return (new_value_binding :: acc))
      (return [])
      value_binding_lst
  in
  return (List.rev rev_value_binding_lst)
;;

let convert_structure structure =
  let helper curr_program = function
    | Simple_ast.SSILet (Ast.Nonrecursive, value_binding_lst) ->
      let* new_value_binding_lst = convert_value_binding_lst value_binding_lst in
      return ((false, new_value_binding_lst) :: curr_program)
    | Simple_ast.SSILet (Ast.Recursive, value_binding_lst) ->
      let* new_value_binding_lst = convert_value_binding_lst value_binding_lst in
      return ((true, new_value_binding_lst) :: curr_program)
    | _ -> return curr_program
  in
  let* rev_program =
    List.fold_left
      (fun acc structure_item ->
        let* curr_program = acc in
        helper curr_program structure_item)
      (return [])
      structure
  in
  return (List.rev rev_program)
;;

let convert (structure : Simple_ast.sstructure) = run (convert_structure structure)

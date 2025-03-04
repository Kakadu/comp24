(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open RS
open RS.RSMonad
open RS.RSMonad.Syntax
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

let rec convert_tup_lst_to_cexpr tup_lst env =
  let helper tup_expr env =
    let* additions, new_expr, env = expr_to_cexpr tup_expr env in
    let* all_additions, fresh_var, env =
      match new_expr with
      | Anf.CImmExpr new_imm_expr -> return (additions, new_imm_expr, env)
      | _ ->
        let* fresh_var, env = checked_fresh_var env in
        return (List.append additions [ fresh_var, new_expr ], Anf.ImmId fresh_var, env)
    in
    return (fresh_var, all_additions, env)
  in
  let* additions, rev_new_tup_lst, env =
    List.fold_left
      (fun acc tup_expr ->
        let* curr_additions, curr_rev_tup_lst, env = acc in
        let* immexpr, new_additions, env = helper tup_expr env in
        return (List.append curr_additions new_additions, immexpr :: curr_rev_tup_lst, env))
      (return ([], [], env))
      tup_lst
  in
  return (additions, Anf.CImmExpr (Anf.ImmTuple (List.rev rev_new_tup_lst)), env)

and convert_app_to_cexpr expr1 expr2 env =
  let rec helper expr1 expr2 env =
    match expr1 with
    | Simple_ast.SEApp (in_expr1, in_expr2) ->
      let* additions, app_var, args, env = helper in_expr1 in_expr2 env in
      let* additions2, new_expr2, env = expr_to_cexpr expr2 env in
      let all_additions = List.append additions additions2 in
      let* all_additions, fresh, env =
        match new_expr2 with
        | Anf.CImmExpr new_imm_expr2 -> return (all_additions, new_imm_expr2, env)
        | _ ->
          let* fresh, env = checked_fresh_var env in
          let all_additions = List.append all_additions [ fresh, new_expr2 ] in
          return (all_additions, Anf.ImmId fresh, env)
      in
      return (all_additions, app_var, List.append args [ fresh ], env)
    | _ ->
      let* additions1, new_expr1, env = expr_to_cexpr expr1 env in
      let* all_additions, fresh1, env =
        match new_expr1 with
        | Anf.CImmExpr (Anf.ImmId var_name) -> return (additions1, var_name, env)
        | _ ->
          let* fresh1, env = checked_fresh_var env in
          let all_additions = List.append additions1 [ fresh1, new_expr1 ] in
          return (all_additions, fresh1, env)
      in
      let* additions2, new_expr2, env = expr_to_cexpr expr2 env in
      let all_additions = List.append all_additions additions2 in
      let* all_additions, fresh2, env =
        match new_expr2 with
        | Anf.CImmExpr new_imm_expr2 -> return (all_additions, new_imm_expr2, env)
        | _ ->
          let* fresh2, env = checked_fresh_var env in
          let all_additions = List.append all_additions [ fresh2, new_expr2 ] in
          return (all_additions, Anf.ImmId fresh2, env)
      in
      return (all_additions, fresh1, [ fresh2 ], env)
  in
  let* all_additions, app_var, args, env = helper expr1 expr2 env in
  return (all_additions, Anf.CApp (app_var, args), env)

and expr_to_cexpr expr env =
  let rec helper env = function
    | Simple_ast.SEConst const ->
      let* const = const_to_immexpr const in
      return ([], Anf.CImmExpr const, env)
    | Simple_ast.SEVar (Ast.Id var) -> return ([], Anf.CImmExpr (ImmId var), env)
    | Simple_ast.SETuple tup_lst -> convert_tup_lst_to_cexpr tup_lst env
    | Simple_ast.SEFun _ -> fail "Unexpected fun"
    | Simple_ast.SELet (Ast.Nonrecursive, value_binding, expr2) ->
      let ident, expr1 = value_binding in
      let* ident_var = convert_ident ident in
      let* additions1, new_expr1, env = helper env expr1 in
      let all_additions = List.append additions1 [ ident_var, new_expr1 ] in
      let* additions2, new_expr2, env = helper env expr2 in
      let all_additions = List.append all_additions additions2 in
      return (all_additions, new_expr2, env)
    | Simple_ast.SELet (Ast.Recursive, _, _) -> fail "Unexpected recursion"
    | Simple_ast.SEApp (expr1, expr2) -> convert_app_to_cexpr expr1 expr2 env
    | Simple_ast.SEIf (expr1, expr2, expr3) ->
      let* additions, new_expr, env = helper env expr1 in
      let* all_additions, fresh_var, env =
        match new_expr with
        | Anf.CImmExpr new_imm_expr -> return (additions, new_imm_expr, env)
        | _ ->
          let* fresh_var, env = checked_fresh_var env in
          let all_additions = List.append additions [ fresh_var, new_expr ] in
          return (all_additions, Anf.ImmId fresh_var, env)
      in
      let* new_expr2, env = expr_to_aexpr expr2 env in
      let* new_expr3, env = expr_to_aexpr expr3 env in
      return (all_additions, Anf.CIf (fresh_var, new_expr2, new_expr3), env)
    | Simple_ast.SECons (expr1, expr2) ->
      let* additions1, new_expr1, env = helper env expr1 in
      let* all_additions, fresh_var1, env =
        match new_expr1 with
        | Anf.CImmExpr new_imm_expr1 -> return (additions1, new_imm_expr1, env)
        | _ ->
          let* fresh_var1, env = checked_fresh_var env in
          let all_additions = List.append additions1 [ fresh_var1, new_expr1 ] in
          return (all_additions, Anf.ImmId fresh_var1, env)
      in
      let* additions2, new_expr2, env = helper env expr2 in
      let all_additions = List.append all_additions additions2 in
      let* all_additions, fresh_var2, env =
        match new_expr2 with
        | Anf.CImmExpr new_imm_expr2 -> return (all_additions, new_imm_expr2, env)
        | _ ->
          let* fresh_var2, env = checked_fresh_var env in
          let all_additions = List.append all_additions [ fresh_var2, new_expr2 ] in
          return (all_additions, Anf.ImmId fresh_var2, env)
      in
      return (all_additions, Anf.CCons (fresh_var1, fresh_var2), env)
  in
  helper env expr

and expr_to_aexpr expr env =
  let rec helper env = function
    | Simple_ast.SELet (Ast.Nonrecursive, value_binding, expr2) ->
      let ident, expr1 = value_binding in
      let* new_ident = convert_ident ident in
      let* additional_bindings1, new_expr1, env = expr_to_cexpr expr1 env in
      let* aexpr2, env = helper env expr2 in
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
      return (new_value_binding_with_additions, env)
    | Simple_ast.SELet (Ast.Recursive, _, _) -> fail "Unexpected rec"
    | _ as expr ->
      let* additional_bindings, new_expr, env = expr_to_cexpr expr env in
      let* res =
        List.fold_left
          (fun acc addition ->
            let* acc = acc in
            let add_ident, add_expr = addition in
            return (Anf.ALet (add_ident, add_expr, acc)))
          (return (Anf.ACExpr new_expr))
          (List.rev additional_bindings)
      in
      return (res, env)
  in
  helper env expr
;;

let convert_value_binding_lst value_binding_lst env =
  let helper value_binding env =
    let ident, expr = value_binding in
    let* new_ident = convert_ident ident in
    let* args_lst, new_expr, env =
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
        let* new_expr, env = expr_to_aexpr fexpr env in
        return (List.rev rev_args, new_expr, env)
      | _ ->
        let* new_expr, env = expr_to_aexpr expr env in
        return ([], new_expr, env)
    in
    return ((new_ident, args_lst, new_expr), env)
  in
  let* rev_value_binding_lst, env =
    List.fold_left
      (fun acc value_binding ->
        let* acc, env = acc in
        let* new_value_binding, env = helper value_binding env in
        return (new_value_binding :: acc, env))
      (return ([], env))
      value_binding_lst
  in
  return (List.rev rev_value_binding_lst, env)
;;

let convert_structure structure env =
  let helper curr_program env = function
    | Simple_ast.SSILet (Ast.Nonrecursive, value_binding_lst) ->
      let* new_value_binding_lst, env = convert_value_binding_lst value_binding_lst env in
      return ((false, new_value_binding_lst) :: curr_program, env)
    | Simple_ast.SSILet (Ast.Recursive, value_binding_lst) ->
      let* new_value_binding_lst, env = convert_value_binding_lst value_binding_lst env in
      return ((true, new_value_binding_lst) :: curr_program, env)
    | _ -> return (curr_program, env)
  in
  let* rev_program, _ =
    List.fold_left
      (fun acc structure_item ->
        let* curr_program, env = acc in
        helper curr_program env structure_item)
      (return ([], env))
      structure
  in
  return (List.rev rev_program)
;;

let convert (structure : Simple_ast.sstructure) (env : VarSSet.t) =
  run (convert_structure structure env)
;;

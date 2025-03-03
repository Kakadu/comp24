(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open S
open S.SMonad
open Containers

type alpha_conversion_setting =
  | All
  | Inner

let checked_fresh_var old_var env =
  match VarSMap.find_opt old_var env with
  | None -> return old_var
  | Some _ ->
    let rec helper env =
      let* new_var = fresh_var in
      match VarSMap.find_opt new_var env with
      | None -> return new_var
      | Some _ -> helper env
    in
    helper env
;;

let get_var_name_from_sid = function
  | Simple_ast.SId (Ast.Id var_name) -> Some var_name
  | Simple_ast.SSpecial SUnit -> None
;;

let rec convert_value_binding_expr env value_binding =
  let ident, expr = value_binding in
  let* expr = convert_expr env expr in
  return (ident, expr)

and convert_expr env expr =
  let rec helper env = function
    | Simple_ast.SEConst _ as contant -> return contant
    | Simple_ast.SEVar (Ast.Id var_name) as var ->
      (match VarSMap.find_opt var_name env with
       | Some x -> return (Simple_ast.SEVar (Ast.Id x))
       | None -> return var)
    | Simple_ast.SETuple expr_lst ->
      let* rev_expr_lst =
        List.fold_left
          (fun acc expr ->
            let* acc = acc in
            let* expr = helper env expr in
            return (expr :: acc))
          (return [])
          expr_lst
      in
      return (Simple_ast.SETuple (List.rev rev_expr_lst))
    | Simple_ast.SEFun (id_lst, expr) ->
      let* rev_new_id_lst, updated_env =
        List.fold_left
          (fun acc id ->
            let* curr_id_lst, curr_env = acc in
            let old_var = get_var_name_from_sid id in
            match old_var with
            | None -> return (id :: curr_id_lst, curr_env)
            | Some old_var ->
              let* new_var = checked_fresh_var old_var curr_env in
              let updated_env = VarSMap.add old_var new_var curr_env in
              return (Simple_ast.SId (Ast.Id new_var) :: curr_id_lst, updated_env))
          (return ([], env))
          id_lst
      in
      let* updated_expr = helper updated_env expr in
      return (Simple_ast.SEFun (List.rev rev_new_id_lst, updated_expr))
    | Simple_ast.SELet (Ast.Nonrecursive, value_binding, expr2) ->
      let old_var_id, expr1 = value_binding in
      let old_var = get_var_name_from_sid old_var_id in
      (match old_var with
       | None ->
         let* new_value_binding = convert_value_binding_expr env value_binding in
         let* new_expr2 = helper env expr2 in
         return (Simple_ast.SELet (Ast.Nonrecursive, new_value_binding, new_expr2))
       | Some old_var ->
         let* new_var = checked_fresh_var old_var env in
         let new_value_binding = Simple_ast.SId (Ast.Id new_var), expr1 in
         let* new_value_binding = convert_value_binding_expr env new_value_binding in
         let updated_env = VarSMap.add old_var new_var env in
         let* new_expr2 = helper updated_env expr2 in
         return (Simple_ast.SELet (Ast.Nonrecursive, new_value_binding, new_expr2)))
    | Simple_ast.SELet (Ast.Recursive, value_binding, expr2) ->
      let old_var_id, expr1 = value_binding in
      let old_var = get_var_name_from_sid old_var_id in
      (match old_var with
       | None ->
         let* new_value_binding = convert_value_binding_expr env value_binding in
         let* new_expr2 = helper env expr2 in
         return (Simple_ast.SELet (Ast.Recursive, new_value_binding, new_expr2))
       | Some old_var ->
         let* new_var = checked_fresh_var old_var env in
         let updated_env = VarSMap.add old_var new_var env in
         let new_value_binding = Simple_ast.SId (Ast.Id new_var), expr1 in
         let* new_value_binding =
           convert_value_binding_expr updated_env new_value_binding
         in
         let* new_expr2 = helper updated_env expr2 in
         return (Simple_ast.SELet (Ast.Recursive, new_value_binding, new_expr2)))
    | Simple_ast.SEApp (expr1, expr2) ->
      let* new_expr1 = helper env expr1 in
      let* new_expr2 = helper env expr2 in
      return (Simple_ast.SEApp (new_expr1, new_expr2))
    | Simple_ast.SEIf (expr1, expr2, expr3) ->
      let* new_expr1 = helper env expr1 in
      let* new_expr2 = helper env expr2 in
      let* new_expr3 = helper env expr3 in
      return (Simple_ast.SEIf (new_expr1, new_expr2, new_expr3))
    | Simple_ast.SECons (expr1, expr2) ->
      let* new_expr1 = helper env expr1 in
      let* new_expr2 = helper env expr2 in
      return (Simple_ast.SECons (new_expr1, new_expr2))
  in
  helper env expr
;;

let update_value_binding_lst_vars env value_binding_lst setting =
  List.fold_left
    (fun acc value_binding ->
      let* value_bindings_with_new_vars, curr_env = acc in
      let old_var_id, expr = value_binding in
      let old_var = get_var_name_from_sid old_var_id in
      match old_var with
      | None -> return (value_binding :: value_bindings_with_new_vars, env)
      | Some old_var ->
        let* new_var =
          match setting with
          | Inner -> return old_var
          | All -> checked_fresh_var old_var curr_env
        in
        let updated_env = VarSMap.add old_var new_var curr_env in
        return
          ( (Simple_ast.SId (Ast.Id new_var), expr) :: value_bindings_with_new_vars
          , updated_env ))
    (return ([], env))
    value_binding_lst
;;

let update_value_binding_lst_exprs env value_binding_lst =
  List.fold_left
    (fun acc value_binding ->
      let* acc = acc in
      let* new_value_binding = convert_value_binding_expr env value_binding in
      return (new_value_binding :: acc))
    (return [])
    value_binding_lst
;;

let convert_structure structure setting =
  let helper env = function
    | Simple_ast.SSILet (rec_flag, value_binding_lst) ->
      let* rev_value_bindings_with_new_vars, updated_env =
        update_value_binding_lst_vars env value_binding_lst setting
      in
      let* new_value_bindings =
        match rec_flag with
        | Ast.Nonrecursive ->
          update_value_binding_lst_exprs env rev_value_bindings_with_new_vars
        | Ast.Recursive ->
          update_value_binding_lst_exprs updated_env rev_value_bindings_with_new_vars
      in
      return (Simple_ast.SSILet (rec_flag, new_value_bindings), updated_env)
    | Simple_ast.SSIExpr expr ->
      let* expr = convert_expr env expr in
      return (Simple_ast.SSIExpr expr, env)
  in
  let* _, rev_structure =
    List.fold_left
      (fun acc structure_item ->
        let* env, si_lst = acc in
        let* new_si, new_env = helper env structure_item in
        return (new_env, new_si :: si_lst))
      (return (VarSMap.empty, []))
      structure
  in
  return (List.rev rev_structure)
;;

let run_alpha_conversion
  (structure : Simple_ast.sstructure)
  (setting : alpha_conversion_setting)
  =
  run (convert_structure structure setting)
;;

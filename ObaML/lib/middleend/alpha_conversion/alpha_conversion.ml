(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open S
open S.SMonad
open Containers

type alpha_conversion_setting =
  | All
  | Inner

let checked_fresh_var old_var env =
  match VarSSet.find_opt old_var env, String.starts_with ~prefix:"#pat#" old_var with
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

let get_var_name_from_sid = function
  | Simple_ast.SId (Ast.Id var_name) -> Some var_name
  | Simple_ast.SSpecial SUnit -> None
;;

let rec convert_value_binding_expr varMap env value_binding =
  let ident, expr = value_binding in
  let* expr, env = convert_expr varMap env expr in
  return ((ident, expr), env)

and convert_expr varMap env expr =
  let rec helper varMap env = function
    | Simple_ast.SEConst _ as contant -> return (contant, env)
    | Simple_ast.SEVar (Ast.Id var_name) as var ->
      (match VarSMap.find_opt var_name varMap with
       | Some x -> return (Simple_ast.SEVar (Ast.Id x), env)
       | None -> return (var, env))
    | Simple_ast.SETuple expr_lst ->
      let* rev_expr_lst, env =
        List.fold_left
          (fun acc expr ->
            let* acc, env = acc in
            let* expr, env = helper varMap env expr in
            return (expr :: acc, env))
          (return ([], env))
          expr_lst
      in
      return (Simple_ast.SETuple (List.rev rev_expr_lst), env)
    | Simple_ast.SEFun (id_lst, expr) ->
      let* rev_new_id_lst, updated_varMap, env =
        List.fold_left
          (fun acc id ->
            let* curr_id_lst, curr_varMap, env = acc in
            let old_var = get_var_name_from_sid id in
            match old_var with
            | None -> return (id :: curr_id_lst, curr_varMap, env)
            | Some old_var ->
              let* new_var, env = checked_fresh_var old_var env in
              let updated_varMap = VarSMap.add old_var new_var curr_varMap in
              return (Simple_ast.SId (Ast.Id new_var) :: curr_id_lst, updated_varMap, env))
          (return ([], varMap, env))
          id_lst
      in
      let* updated_expr, env = helper updated_varMap env expr in
      return (Simple_ast.SEFun (List.rev rev_new_id_lst, updated_expr), env)
    | Simple_ast.SELet (Ast.Nonrecursive, value_binding, expr2) ->
      let old_var_id, expr1 = value_binding in
      let old_var = get_var_name_from_sid old_var_id in
      (match old_var with
       | None ->
         let* new_value_binding, env =
           convert_value_binding_expr varMap env value_binding
         in
         let* new_expr2, env = helper varMap env expr2 in
         return (Simple_ast.SELet (Ast.Nonrecursive, new_value_binding, new_expr2), env)
       | Some old_var ->
         let* new_var, env = checked_fresh_var old_var env in
         let new_value_binding = Simple_ast.SId (Ast.Id new_var), expr1 in
         let* new_value_binding, env =
           convert_value_binding_expr varMap env new_value_binding
         in
         let updated_varMap = VarSMap.add old_var new_var varMap in
         let* new_expr2, env = helper updated_varMap env expr2 in
         return (Simple_ast.SELet (Ast.Nonrecursive, new_value_binding, new_expr2), env))
    | Simple_ast.SELet (Ast.Recursive, value_binding, expr2) ->
      let old_var_id, expr1 = value_binding in
      let old_var = get_var_name_from_sid old_var_id in
      (match old_var with
       | None ->
         let* new_value_binding, env =
           convert_value_binding_expr varMap env value_binding
         in
         let* new_expr2, env = helper varMap env expr2 in
         return (Simple_ast.SELet (Ast.Recursive, new_value_binding, new_expr2), env)
       | Some old_var ->
         let* new_var, env = checked_fresh_var old_var env in
         let updated_varMap = VarSMap.add old_var new_var varMap in
         let new_value_binding = Simple_ast.SId (Ast.Id new_var), expr1 in
         let* new_value_binding, env =
           convert_value_binding_expr updated_varMap env new_value_binding
         in
         let* new_expr2, env = helper updated_varMap env expr2 in
         return (Simple_ast.SELet (Ast.Recursive, new_value_binding, new_expr2), env))
    | Simple_ast.SEApp (expr1, expr2) ->
      let* new_expr1, env = helper varMap env expr1 in
      let* new_expr2, env = helper varMap env expr2 in
      return (Simple_ast.SEApp (new_expr1, new_expr2), env)
    | Simple_ast.SEIf (expr1, expr2, expr3) ->
      let* new_expr1, env = helper varMap env expr1 in
      let* new_expr2, env = helper varMap env expr2 in
      let* new_expr3, env = helper varMap env expr3 in
      return (Simple_ast.SEIf (new_expr1, new_expr2, new_expr3), env)
    | Simple_ast.SECons (expr1, expr2) ->
      let* new_expr1, env = helper varMap env expr1 in
      let* new_expr2, env = helper varMap env expr2 in
      return (Simple_ast.SECons (new_expr1, new_expr2), env)
  in
  helper varMap env expr
;;

let update_value_binding_lst_vars varMap env value_binding_lst setting =
  List.fold_left
    (fun acc value_binding ->
      let* value_bindings_with_new_vars, curr_varMap, env = acc in
      let old_var_id, expr = value_binding in
      let old_var = get_var_name_from_sid old_var_id in
      match old_var with
      | None -> return (value_binding :: value_bindings_with_new_vars, curr_varMap, env)
      | Some old_var ->
        let* new_var, env =
          match setting with
          | Inner -> return (old_var, VarSSet.add old_var env)
          | All -> checked_fresh_var old_var env
        in
        let updated_varMap = VarSMap.add old_var new_var curr_varMap in
        return
          ( (Simple_ast.SId (Ast.Id new_var), expr) :: value_bindings_with_new_vars
          , updated_varMap
          , env ))
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

let convert_structure structure setting =
  let helper varMap env = function
    | Simple_ast.SSILet (rec_flag, value_binding_lst) ->
      let* rev_value_bindings_with_new_vars, updated_varMap, env =
        update_value_binding_lst_vars varMap env value_binding_lst setting
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
      return (Simple_ast.SSILet (rec_flag, new_value_bindings), updated_varMap, env)
    | Simple_ast.SSIExpr expr ->
      let* expr, env = convert_expr varMap env expr in
      return (Simple_ast.SSIExpr expr, varMap, env)
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

let run_alpha_conversion
  (structure : Simple_ast.sstructure)
  (setting : alpha_conversion_setting)
  =
  run (convert_structure structure setting)
;;

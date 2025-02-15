(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Common.StateMonad
open Common.StateMonad.Syntax
open Common.IdentifierStructs
open IdentifierSearcher
open IdentifierSubstitutor

let rec closure_expression env fv_map = function
  | EIdentifier _ as expr -> closure_identifier env fv_map expr
  | EFun _ as expr ->
    let* fun_closure, _ =
      closure_fun true env fv_map (FunctionTransformer.transform_fun expr)
    in
    return fun_closure
  | ELetIn (case, cases, expr) -> closure_let_in env fv_map (case, cases) expr
  | ERecLetIn (case, cases, expr) ->
    closure_rec_let_in env fv_map (case, cases) expr IdentifierMap.empty
  | EApplication (func, arg1, args) -> closure_application env fv_map (func, arg1, args)
  | EIfThenElse (cond, b1, b2) -> closure_if_then_else env fv_map (cond, b1, b2)
  | ETuple (e1, e2, es) -> closure_tuple env fv_map (e1, e2, es)
  | EListConstructor (l, r) -> closure_list_constructor env fv_map (l, r)
  | EFunction (case, cases) -> closure_function env fv_map (case, cases)
  | EMatchWith (expr, case, cases) -> closure_match_with env fv_map expr (case, cases)
  | ETyped (e, typ) -> closure_typed_expression env fv_map (e, typ)
  | expr -> return expr

and closure_identifier env fv_map = function
  | EIdentifier id as expr ->
    (match IdentifierMap.find_opt id fv_map with
     | None -> return expr
     | Some free_vars ->
       let args =
         IdentifierSet.fold (fun var acc -> EIdentifier var :: acc) free_vars []
       in
       (match args with
        | [] -> return expr
        | hd :: tl -> return @@ EApplication (expr, hd, tl)))
  | expr -> closure_expression env fv_map expr

and closure_fun application_flag env fv_map = function
  (* If [application_flag] is true, it adds application with new arguments around the new function *)
  | EFun ((p, ps), body) as expr ->
    let patters = p :: ps in
    let free_vars = get_expr_free_vars expr in
    let free_vars = IdentifierSet.diff free_vars env in
    let pattern_identifiers = get_pattern_identifiers_from_list patters in
    let fv_map = remove_keys_from_map pattern_identifiers fv_map in
    let* new_args, replacement_map =
      IdentifierSet.fold
        (fun var acc ->
          let* args, map = acc in
          let patterns_identifiers =
            List.fold_left
              (fun acc p -> IdentifierSet.union acc (get_pattern_identifiers p))
              IdentifierSet.empty
              patters
          in
          let common_env = get_expr_free_vars expr in
          let common_env = IdentifierSet.union common_env patterns_identifiers in
          let common_env = IdentifierSet.union common_env env in
          let* new_name = NameCreator.get_new_arg_name common_env in
          let new_id = Id new_name in
          return (IdentifierSet.add new_id args, IdentifierMap.add var new_id map))
        free_vars
        (return (IdentifierSet.empty, IdentifierMap.empty))
    in
    let old_free_vars =
      IdentifierSet.fold (fun var acc -> EIdentifier var :: acc) free_vars []
    in
    let closed_vars =
      IdentifierSet.fold (fun var acc -> PVar var :: acc) new_args patters
    in
    let* closure_body = closure_expression env fv_map body in
    let updated_body = substitute_identifiers replacement_map closure_body in
    (match closed_vars with
     | [] -> return (expr, replacement_map)
     | head :: tail ->
       (match old_free_vars, application_flag with
        | [], _ | _, false ->
          return @@ (EFun ((head, tail), updated_body), replacement_map)
        | hd :: tl, true ->
          return
          @@ (EApplication (EFun ((head, tail), updated_body), hd, tl), replacement_map)))
  | expr ->
    let* closure_expr = closure_expression env fv_map expr in
    return @@ (closure_expr, IdentifierMap.empty)

and closure_application env fv_map (func, arg1, args) =
  let* func_closure = closure_expression env fv_map func in
  let* arg1_closure = closure_expression env fv_map arg1 in
  let* args_closure =
    List.fold_left
      (fun acc arg ->
        let* acc = acc in
        let* closed_arg = closure_expression env fv_map arg in
        let new_acc = closed_arg :: acc in
        return new_acc)
      (return [])
      args
  in
  let args_closure = List.rev args_closure in
  return @@ EApplication (func_closure, arg1_closure, args_closure)

and closure_if_then_else env fv_map (cond, b1, b2) =
  let* closure_cond = closure_expression env fv_map cond in
  let* closure_b1 = closure_expression env fv_map b1 in
  let* closure_b2 =
    match b2 with
    | Some expr ->
      let* closure_b2 = closure_expression env fv_map expr in
      return @@ Some closure_b2
    | None -> return None
  in
  return @@ EIfThenElse (closure_cond, closure_b1, closure_b2)

and closure_tuple env fv_map (e1, e2, es) =
  let* e1 = closure_expression env fv_map e1 in
  let* e2 = closure_expression env fv_map e2 in
  let* es =
    List.fold_left
      (fun acc arg ->
        let* acc = acc in
        let* closed_expr = closure_expression env fv_map arg in
        return (closed_expr :: acc))
      (return [])
      es
  in
  return @@ ETuple (e1, e2, List.rev es)

and closure_list_constructor env fv_map (l, r) =
  let* l = closure_expression env fv_map l in
  let* r = closure_expression env fv_map r in
  return @@ EListConstructor (l, r)

and closure_case env fv_map ((p1, e1), cases) =
  let* case =
    let* closure_e1 = closure_expression env fv_map e1 in
    return (p1, closure_e1)
  in
  let* cases =
    List.fold_left
      (fun acc (p, e) ->
        let* acc = acc in
        let* closure_e = closure_expression env fv_map e in
        let new_acc = (p, closure_e) :: acc in
        return new_acc)
      (return [])
      cases
  in
  let cases = List.rev cases in
  return (case, cases)

and closure_match_with env fv_map expr (case, cases) =
  let* main_expr = closure_expression env fv_map expr in
  let* case, cases = closure_case env fv_map (case, cases) in
  return @@ EMatchWith (main_expr, case, cases)

and closure_function env fv_map (case, cases) =
  let* common_env =
    List.fold_left
      (fun acc (_, e) ->
        let* acc = acc in
        let e_free_vars = get_expr_free_vars e in
        return @@ IdentifierSet.union acc e_free_vars)
      (return env)
      (case :: cases)
  in
  let pattern_identifiers = get_pattern_identifiers_from_cases (case :: cases) in
  let common_env = IdentifierSet.union common_env pattern_identifiers in
  let* new_name = NameCreator.get_new_arg_name common_env in
  let* case, cases = closure_case env fv_map (case, cases) in
  let new_pattern, new_identifier = PVar (Id new_name), EIdentifier (Id new_name) in
  let new_expr = EFun ((new_pattern, []), EMatchWith (new_identifier, case, cases)) in
  closure_expression env fv_map new_expr

and closure_typed_expression env fv_map (e, typ) =
  let* closure_e = closure_expression env fv_map e in
  return @@ ETyped (closure_e, typ)

and closure_let_in env fv_map (case, cases) expr =
  let pattern_identifiers = get_pattern_identifiers_from_cases (case :: cases) in
  let env_for_decl_transform = IdentifierSet.union env pattern_identifiers in
  let* transformed_decl =
    FunctionTransformer.transform_let_in
      env_for_decl_transform
      (ELetIn (case, cases, expr))
  in
  match transformed_decl with
  | ELetIn (case, cases, expr) ->
    let cases = case :: cases in
    let updated_fv_map =
      List.fold_left
        (fun acc (p, expr) ->
          let free_vars = IdentifierSet.diff (get_expr_free_vars expr) env in
          match p, expr with
          | PVar id, EFun _ -> IdentifierMap.add id free_vars acc
          | _ -> acc)
        fv_map
        cases
    in
    let* closed_cases =
      List.fold_left
        (fun acc (p, expr) ->
          let* acc = acc in
          match expr with
          | EFun _ ->
            let* closed_expr, _ = closure_fun false env fv_map expr in
            return ((p, closed_expr) :: acc)
          | _ ->
            let* closed_expr = closure_expression env fv_map expr in
            return ((p, closed_expr) :: acc))
        (return [])
        cases
    in
    let closed_cases = List.rev closed_cases in
    let* closed_body = closure_expression env updated_fv_map expr in
    return @@ ELetIn (List.hd closed_cases, List.tl closed_cases, closed_body)
  | expr -> return expr

and closure_rec_let_in env fv_map (case, cases) expr previous_replacement_maps =
  let apply_replacement_map_to_cases replacement_maps cases =
    List.map
      (fun (p, expr) ->
        match p with
        | PVar i ->
          (match IdentifierMap.find_opt i replacement_maps with
           | Some replacement_map -> p, substitute_identifiers replacement_map expr
           | None -> p, expr)
        | _ -> p, expr)
      cases
  in
  let cases = case :: cases in
  let pattern_identifiers = get_pattern_identifiers_from_cases cases in
  let extended_env = IdentifierSet.union env pattern_identifiers in
  let updated_fv_map =
    List.fold_left
      (fun acc (p, expr) ->
        let free_vars = IdentifierSet.diff (get_expr_free_vars expr) extended_env in
        match p, expr with
        | PVar id, EFun _ -> IdentifierMap.add id free_vars acc
        | _ -> acc)
      fv_map
      cases
  in
  let* closed_cases, current_replacement_maps =
    List.fold_left
      (fun acc (p, expr) ->
        let* acc_cases, acc_replacement_map = acc in
        match p, expr with
        | PVar i, EFun _ ->
          let* closed_expr, replacement_map =
            closure_fun false extended_env updated_fv_map expr
          in
          return
            ( (p, closed_expr) :: acc_cases
            , IdentifierMap.add i replacement_map acc_replacement_map )
        | _ ->
          let* closed_expr = closure_expression extended_env updated_fv_map expr in
          return ((p, closed_expr) :: acc_cases, acc_replacement_map))
      (return ([], IdentifierMap.empty))
      cases
  in
  let closed_cases = List.rev closed_cases in
  let closed_cases =
    apply_replacement_map_to_cases previous_replacement_maps closed_cases
  in
  let closed_cases =
    apply_replacement_map_to_cases current_replacement_maps closed_cases
  in
  let* closed_body = closure_expression env updated_fv_map expr in
  let case_contains_fv_predicate (_, expr) =
    not
      (IdentifierSet.is_empty (IdentifierSet.diff (get_expr_free_vars expr) extended_env))
  in
  if List.exists case_contains_fv_predicate cases
  then
    closure_rec_let_in
      env
      fv_map
      (List.hd closed_cases, List.tl closed_cases)
      closed_body
      current_replacement_maps
  else return @@ ERecLetIn (List.hd closed_cases, List.tl closed_cases, closed_body)
;;

open Ast
open Base

let find_unbound_vars expr =
  let rec explore_expr exp bound =
    match exp with
    | EConst _ -> Set.empty (module String)
    | EVar var_name -> if Set.mem bound var_name then Set.empty (module String) else Set.singleton (module String) var_name
    | EFun (pattern, body) ->
        explore_expr body (bind_pattern_vars pattern bound)
    | EApply (func, arg) ->
        Set.union (explore_expr func bound) (explore_expr arg bound)
    | EIf (cond, then_exp, else_exp) ->
        Set.union_list (module String) [
          explore_expr cond bound;
          explore_expr then_exp bound;
          explore_expr else_exp bound
        ]
    | ELet (_is_rec, bindings, body) ->
        let patterns_set =
          List.fold bindings ~init:(Set.empty (module String))
            ~f:(fun acc (pattern, _) -> Set.union acc (get_pattern_vars pattern))
        in
        let bindings_unbound =
          List.fold bindings ~init:(Set.empty (module String))
            ~f:(fun acc (_, bind_expr) -> Set.union acc (explore_expr bind_expr bound))
        in
        let body_unbound = explore_expr body (Set.union bound patterns_set) in
        let bound_vars = List.fold bindings ~init:(Set.empty (module String)) ~f:(fun acc (_, expr) ->
          Set.union acc (nested_func_binds expr))
        in
        Set.diff (Set.union bindings_unbound body_unbound) bound_vars
    | ETuple elements ->
        List.fold elements ~init:(Set.empty (module String)) ~f:(fun acc e -> Set.union acc (explore_expr e bound))
    | EMatch (matched_expr, branches) ->
        let unbound_in_match = explore_expr matched_expr bound in
        let unbound_in_cases =
          List.fold branches ~init:(Set.empty (module String)) ~f:(fun acc (pattern, case_body) ->
            Set.union acc (explore_expr case_body (bind_pattern_vars pattern bound)))
        in
        Set.union unbound_in_match unbound_in_cases
    | ECons (head, tail) ->
        Set.union (explore_expr head bound) (explore_expr tail bound)
  and bind_pattern_vars pattern bound_set =
    match pattern with
    | PAny | PConst _ -> bound_set
    | PVar id -> Set.add bound_set id
    | PCons (left, right) -> bind_pattern_vars right (bind_pattern_vars left bound_set)
    | PTuple pats -> List.fold pats ~init:bound_set ~f:(fun acc pat -> bind_pattern_vars pat acc)
    | PConstraint (nested_pat, _) -> bind_pattern_vars nested_pat bound_set
  and get_pattern_vars pat =
    match pat with
    | PVar id -> Set.singleton (module String) id
    | PTuple pats -> List.fold pats ~init:(Set.empty (module String)) ~f:(fun acc pat -> Set.union acc (get_pattern_vars pat))
    | PCons (p1, p2) -> Set.union (get_pattern_vars p1) (get_pattern_vars p2)
    | PConstraint (pat, _) -> get_pattern_vars pat
    | PAny | PConst _ -> Set.empty (module String)
  and nested_func_binds func_expr =
    match func_expr with
    | EFun (pat, body) -> Set.union (get_pattern_vars pat) (nested_func_binds body)
    | _ -> Set.empty (module String)
  in
  explore_expr expr (Set.empty (module String))

let rec close_function_scope local_defs local_ctx global_ctx converter expr =
  match expr with
  | EFun (pattern, body) ->
      let closed_body = close_function_scope local_defs local_ctx global_ctx converter body in
      EFun (pattern, closed_body)
  | _ -> converter local_defs local_ctx global_ctx expr

let find_global_vars pattern =
  let rec collect_globals pat =
    match pat with
    | PVar id -> Set.singleton (module String) id
    | PTuple pats -> List.fold pats ~init:(Set.empty (module String)) ~f:(fun acc p -> Set.union acc (collect_globals p))
    | PCons (p1, p2) -> Set.union (collect_globals p1) (collect_globals p2)
    | PConstraint (nested_pat, _) -> collect_globals nested_pat
    | PAny | PConst _ -> Set.empty (module String)
  in
  collect_globals pattern

let ops_set =
  Set.of_list (module String) ["(+)"; "(::)"; "(*)"; "(-)"; "(==)"; "(=)"; "(/)"]

let rec transform_ast global_ctx structure_item =
  let rec process_expression locals ctx global_ctx expr =
    match expr with
    | EConst _ as const -> const
    | EVar id ->
        (match Map.find ctx id with
        | None -> EVar id
        | Some free_vars ->
            List.fold_left (Set.to_list free_vars) ~init:(EVar id) ~f:(fun acc var -> EApply (acc, EVar var)))
    | EFun (pat, body) ->
        let unbound_vars = find_unbound_vars (EFun (pat, body)) in
        let free_vars = Set.diff unbound_vars global_ctx |> Set.diff ops_set in
        let variables = Set.to_list free_vars |> List.map ~f:(fun v -> PVar v) in
        let wrapped_vars =
          List.fold_right variables ~init:(close_function_scope locals ctx global_ctx process_expression body)
            ~f:(fun pat wrapped -> EFun (pat, wrapped))
        in
        List.fold_left (Set.to_list free_vars) ~init:wrapped_vars ~f:(fun acc var -> EApply (acc, EVar var))
    | EApply (f, arg) ->
        let new_global_ctx = Set.diff global_ctx ops_set in
        EApply (process_expression locals ctx new_global_ctx f, process_expression locals ctx new_global_ctx arg)
    | EIf (cond, then_branch, else_branch) ->
        let new_global_ctx = Set.diff global_ctx ops_set in
        EIf (
          process_expression locals ctx new_global_ctx cond,
          process_expression locals ctx new_global_ctx then_branch,
          process_expression locals ctx new_global_ctx else_branch
        )
    | ELet (is_rec, bindings, body) ->
        let updated_ctx, updated_defs, bindings_converted =
          List.fold bindings ~init:(ctx, locals, []) ~f:(fun (ctx_acc, defs_acc, bind_acc) (pat, exp) ->
            let free_vars = find_unbound_vars exp |> Set.diff global_ctx |> Set.diff ops_set in
            let closed_exp = close_function_scope defs_acc ctx_acc global_ctx process_expression exp in
            let ctx_updated = Map.set ctx_acc ~key:(match pat with PVar id -> id | _ -> "") ~data:free_vars in
            (ctx_updated, Set.add defs_acc "", (pat, closed_exp) :: bind_acc))
        in
        let new_global_ctx = Set.diff global_ctx ops_set in
        let closed_body = process_expression updated_defs updated_ctx new_global_ctx body in
        ELet (is_rec, List.rev bindings_converted, closed_body)
    | _ -> expr
  in
  match structure_item with
  | SEval expr ->
      SEval (process_expression (Set.empty (module String)) (Map.empty (module String)) global_ctx expr)
  | SValue (rec_flag, bindings) ->
      let bindings_converted =
        List.map bindings ~f:(fun (pat, exp) ->
          let new_exp =
            close_function_scope (Set.empty (module String)) (Map.empty (module String)) global_ctx process_expression exp
          in
          (pat, new_exp))
      in
      SValue (rec_flag, bindings_converted)

let convert_all_ast input_ast =
  let rec process_items ast_items global_ctx acc =
    match ast_items with
    | [] -> List.rev acc
    | item :: rest ->
        let updated_ctx = 
          match item with
          | SValue (_, binds) ->
              List.fold binds ~init:global_ctx ~f:(fun acc_globals (pat, _) -> Set.union acc_globals (find_global_vars pat))
          | SEval _ -> global_ctx
        in
        process_items rest updated_ctx ((transform_ast global_ctx item) :: acc)
  in
  process_items input_ast (Set.empty (module String)) []

let run_tests ast =
  let transformed = convert_all_ast ast in
  Stdlib.Format.printf "%s" (show_structure transformed)
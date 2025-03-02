open Pat_elim_ast
open Common

module StringMap = Map.Make (String)

let builtin_list =
  [ "( + )"; "( - )"; "( / )"; "( * )"
  ; "( < )"; "( > )"; "( <= )"; "( >= )"
  ; "( <> )"; "( = )"; "print_int" ]

let empty_env = StringMap.empty

let update_maps primary secondary =
  StringMap.union (fun _ _ new_val -> Some new_val) primary secondary

let rec free_vars env expr =
  let union_maps = List.fold_left (fun acc map -> update_maps acc map) empty_env in
  match expr with
  | PEEConst _ -> empty_env
  | PEEVar id ->
    if StringMap.mem id env
      then empty_env
      else StringMap.singleton id ()
  | PEEIf (cond, e1, e2) ->
    union_maps [ free_vars env cond; free_vars env e1; free_vars env e2 ]
  | PEEFun (args, body_expr) ->
    let env' = List.fold_left (fun acc arg -> StringMap.add arg () acc) env args in
    free_vars env' body_expr
  | PEEApp (fn, arg) ->
    update_maps (free_vars env fn) (free_vars env arg)
  | PEELet (PENonrec (vname, val_expr), in_expr) ->
    let val_fvs = free_vars env val_expr in
    let env' = StringMap.add vname () env in
    update_maps val_fvs (free_vars env' in_expr)
  | PEELet (PERec defs, in_expr) ->
    let def_names = List.map fst defs in
    let env_with_names = List.fold_left (fun acc name -> StringMap.add name () acc) env def_names in
    let def_fvs = List.fold_left (fun acc (_, expr) -> update_maps acc (free_vars env_with_names expr)) empty_env defs in
    update_maps def_fvs (free_vars env_with_names in_expr)
  | PEECons (first, rest) ->
    update_maps (free_vars env first) (free_vars env rest)
  | PEETuple elems ->
    union_maps (List.map (free_vars env) elems)

let expr_apply_args expr args env =
  List.fold_left (fun acc_expr arg_name ->
    let arg_expr = if StringMap.mem arg_name env then StringMap.find arg_name env else PEEVar arg_name in
    PEEApp (acc_expr, arg_expr)) expr args

let rec rewrite_expr global_env local_env expr =
  match expr with
  | PEEConst _ as c -> c
  | PEEVar x as v -> (match StringMap.find_opt x local_env with Some e -> e | None -> v)
  | PEEIf (cond, e_true, e_false) ->
    let cond' = rewrite_expr global_env local_env cond in
    let e_true' = rewrite_expr global_env local_env e_true in
    let e_false' = rewrite_expr global_env local_env e_false in
    PEEIf (cond', e_true', e_false')
  | PEEFun (params, body_expr) as original_fun ->
    let fv_map = free_vars global_env original_fun in
    let fv_list = StringMap.bindings fv_map |> List.map fst in
    let body_new = rewrite_expr global_env empty_env body_expr in
    let transformed_fun = PEEFun (fv_list @ params, body_new) in
    expr_apply_args transformed_fun fv_list local_env
  | PEEApp (fn, arg) ->
    let fn' = rewrite_expr global_env local_env fn in
    let arg' = rewrite_expr global_env local_env arg in
    PEEApp (fn', arg')
  | PEELet (PENonrec (id, bind_expr), in_expr) ->
    let bind_expr', updated_env = rewrite_nonrec global_env local_env id bind_expr in
    let local_env = update_maps local_env updated_env in
    let in_expr' = rewrite_expr global_env local_env in_expr in
    PEELet (PENonrec (id, bind_expr'), in_expr')
  | PEELet (PERec defs, in_expr) ->
    let rewritten_defs, updated_env = rewrite_recursive global_env local_env defs in
    let local_env = update_maps local_env updated_env in
    let in_expr' = rewrite_expr global_env local_env in_expr in
    PEELet (PERec rewritten_defs, in_expr')
  | PEECons (hd, tl) ->
    PEECons (rewrite_expr global_env local_env hd, rewrite_expr global_env local_env tl)
  | PEETuple elems ->
    PEETuple (List.map (rewrite_expr global_env local_env) elems)

and rewrite_nonrec global_env local_env var expr =
  match expr with
  | PEEFun (params, body_expr) ->
    let body_fvs = free_vars global_env body_expr |> StringMap.bindings |> List.map fst in
    let body_transformed = rewrite_expr global_env empty_env body_expr in
    let transformed_fun = PEEFun (body_fvs @ params, body_transformed) in
    let new_binding = expr_apply_args (PEEVar var) body_fvs local_env in
    transformed_fun, StringMap.singleton var new_binding
  | _ ->
    rewrite_expr global_env local_env expr, empty_env

and rewrite_recursive global_env prev_env defs=
  let def_names = List.map fst defs in
  let gather_fvs (fvs_acc, env_acc) (id, expr) =
    match expr with
    | PEEFun (args, body_expr) ->
      let exclude_names = args @ def_names in
      let exclude_map = List.fold_left (fun acc n -> StringMap.add n () acc) empty_env exclude_names in
      let body_fvs = free_vars global_env body_expr |> StringMap.filter (fun id _ -> not (StringMap.mem id exclude_map)) in
      let fv_names = StringMap.bindings body_fvs |> List.map fst in
      let new_expr = expr_apply_args (PEEVar id) fv_names prev_env in
      (fv_names :: fvs_acc), StringMap.add id new_expr env_acc
    | _ -> []::fvs_acc, env_acc
  in
  let fv_lists, combined_env = List.fold_left gather_fvs ([], prev_env) defs in
  let new_defs = List.map2 (fun (id,expr) fv_names ->
    match expr with
    | PEEFun (args, body_expr) ->
      let body_transformed = rewrite_expr global_env empty_env body_expr in
      (id, PEEFun (fv_names @ args, body_transformed))
    | _ -> (id, rewrite_expr global_env combined_env expr)
  ) defs (List.rev fv_lists) in
  new_defs, combined_env

let transform_toplevel global_env = function
  | PENonrec (var, expr) ->
    let transformed_expr, _ = rewrite_nonrec global_env empty_env var expr in
    StringMap.add var () global_env, PENonrec (var, transformed_expr)
  | PERec defs ->
    let vars = List.map fst defs in
    let transformed_defs, _ = rewrite_recursive global_env empty_env defs in
    let global_env = List.fold_left (fun env name -> StringMap.add name () env) global_env vars in
    global_env, PERec transformed_defs

let initial_env =
  List.fold_left (fun acc name -> StringMap.add name () acc) empty_env builtin_list

let perform_closure_conversion prog =
  let rec loop env acc = function
    | [] -> List.rev acc
    | decl :: rest ->
      let env', decl' = transform_toplevel env decl in
      loop env' (decl' :: acc) rest
  in
  loop initial_env [] prog
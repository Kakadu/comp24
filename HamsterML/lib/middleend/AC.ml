open Ast
open Base
module Format = Stdlib.Format
open Utils.R

module NameEnv = struct
  include Utils.NameEnv

  (* generates name with overlap *)
  let rec generate_new_name (env : t) (name : id) (prefix : string) =
    let* fresh_num = fresh in
    let new_name = prefix ^ Int.to_string fresh_num in
    match find new_name env with
    | None -> return (extend (name, new_name) env, new_name)
    | Some _ -> generate_new_name env name prefix
  ;;

  (* generates new name or returns old one if it exists *)
  let resolve_name (env : t) (name : id) (prefix : string) =
    match find name env with
    | None -> generate_new_name env name prefix
    | Some old_name -> return (env, old_name)
  ;;
end

let rec convert_pattern (env : NameEnv.t) prefix = function
  | (Const _ | Wildcard | Operation (Unary _)) as orig -> return (env, orig)
  | Var id ->
    (* try to find name or create new one *)
    let* env, new_id = NameEnv.resolve_name env id prefix in
    return (env, Var new_id)
  | Operation (Binary op) ->
    let op_id = BinOperator.to_string op in
    let* env, new_id = NameEnv.resolve_name env op_id prefix in
    return (env, Var new_id)
  | Constraint (p, dt) ->
    let* env, ac_p = convert_pattern env prefix p in
    return (env, Constraint (ac_p, dt))
  | List pts ->
    let* env, ac_pts =
      fold_list pts ~init:(env, []) ~f:(fun (env, acc) p ->
        let* env, ac_p = convert_pattern env prefix p in
        return (env, ac_p :: acc))
    in
    let ac_pts = List.rev ac_pts in
    return (env, List ac_pts)
  | ListConcat (p1, p2) ->
    let* env, ac_p1 = convert_pattern env prefix p1 in
    let* env, ac_p2 = convert_pattern env prefix p2 in
    return (env, ListConcat (ac_p1, ac_p2))
  | Tuple (a, b, pts) ->
    let* env, ac_a = convert_pattern env prefix a in
    let* env, ac_b = convert_pattern env prefix b in
    let* env, ac_pts =
      fold_list pts ~init:(env, []) ~f:(fun (env, acc) p ->
        let* env, ac_p = convert_pattern env prefix p in
        return (env, ac_p :: acc))
    in
    let ac_pts = List.rev ac_pts in
    return (env, Tuple (ac_a, ac_b, ac_pts))
;;

let convert_name_pattern (env : NameEnv.t) (arg : pattern) (name_prefix : string) =
  match arg with
  | Var id ->
    (* creare new name anyway *)
    let* env, new_id = NameEnv.generate_new_name env id name_prefix in
    return (env, Var new_id)
  | Operation (Binary op) ->
    let op_id = BinOperator.to_string op in
    let* env, new_id = NameEnv.resolve_name env op_id name_prefix in
    return (env, Var new_id)
  | arg -> convert_pattern env name_prefix arg
;;

let arg_prefix = "arg_"
let let_prefix = "var_"

let collect_args (env : NameEnv.t) (args : args) =
  let* env, ac_args =
    fold_list args ~init:(env, []) ~f:(fun (env, acc) arg ->
      let* env, ac_arg = convert_name_pattern env arg arg_prefix in
      return (env, ac_arg :: acc))
  in
  return (env, List.rev ac_args)
;;

let collect_mutual_names (env : NameEnv.t) (binds : bind list) =
  fold_list binds ~init:env ~f:(fun env (name, _, _) ->
    let* env, _ = convert_name_pattern env name let_prefix in
    return env)
;;

let rec convert_bind_nonrec (env : NameEnv.t) ((name, args, body) : bind) =
  let* args_env, ac_args = collect_args env args in
  let* _, ac_body = convert_expr args_env body in
  let* name_env, new_name = convert_name_pattern env name let_prefix in
  return (name_env, ((new_name, ac_args, ac_body) : bind))

and convert_bind_rec (env : NameEnv.t) ((name, args, body) : bind) =
  (* we add names during "collect mutual names", convert_pattern just gets them *)
  let* name_env, new_name = convert_pattern env let_prefix name in
  let* args_env, ac_args = collect_args name_env args in
  let* _, ac_body = convert_expr args_env body in
  return (env, ((new_name, ac_args, ac_body) : bind))

and convert_expr (env : NameEnv.t) = function
  | (EConst _ | EOperation (Unary _)) as orig -> return (env, orig)
  | EOperation (Binary op as bop) ->
    let op_id = BinOperator.to_string op in
    (match NameEnv.find op_id env with
     | Some new_id -> return (env, EVar new_id)
     | None -> return (env, EOperation bop))
  | EVar id ->
    (match NameEnv.find id env with
     | Some new_id -> return (env, EVar new_id)
     | None -> return (env, EVar id))
  | Application (l, r) ->
    let* env, ac_l = convert_expr env l in
    let* env, ac_r = convert_expr env r in
    return (env, Application (ac_l, ac_r))
  | Fun (args, body) ->
    let* args_env, ac_args = collect_args env args in
    let* _, ac_body = convert_expr args_env body in
    return (env, Fun (ac_args, ac_body))
  | Let (rec_flag, binds, scope) ->
    let* env, convert_bind =
      match rec_flag with
      | Recursive ->
        let* env = collect_mutual_names env binds in
        return (env, convert_bind_rec)
      | Nonrecursive -> return (env, convert_bind_nonrec)
    in
    let* env, new_binds =
      fold_list binds ~init:(env, []) ~f:(fun (env, acc_binds) bnd ->
        let* env, ac_bnd = convert_bind env bnd in
        return (env, ac_bnd :: acc_binds))
    in
    let new_binds = List.rev new_binds in
    (match scope with
     | None -> return (env, Let (rec_flag, new_binds, None))
     | Some scope ->
       let* env, ac_scope = convert_expr env scope in
       return (env, Let (rec_flag, new_binds, Some ac_scope)))
  | EList exprs ->
    let* ac_exprs =
      map_list exprs ~f:(fun expr -> convert_expr env expr >>| fun (_, expr) -> expr)
    in
    return (env, EList ac_exprs)
  | EListConcat (l, r) ->
    let* _, ac_l = convert_expr env l in
    let* _, ac_r = convert_expr env r in
    return (env, EListConcat (ac_l, ac_r))
  | ETuple (a, b, tl) ->
    let* _, ac_a = convert_expr env a in
    let* _, ac_b = convert_expr env b in
    let* ac_tl =
      map_list tl ~f:(fun expr -> convert_expr env expr >>| fun (_, expr) -> expr)
    in
    return (env, ETuple (ac_a, ac_b, ac_tl))
  | EConstraint (expr, dt) ->
    let* _, ac_expr = convert_expr env expr in
    return (env, EConstraint (ac_expr, dt))
  | If (_if, _then, _else) ->
    let* _, ac_if = convert_expr env _if in
    let* _, ac_then = convert_expr env _then in
    (match _else with
     | Some _else ->
       let* _, ac_else = convert_expr env _else in
       return (env, If (ac_if, ac_then, Some ac_else))
     | None -> return (env, If (ac_if, ac_then, None)))
  | Match (expr, cases) ->
    let* _, ac_expr = convert_expr env expr in
    let* ac_cases =
      fold_list cases ~init:[] ~f:(fun acc (p, expr) ->
        let* env, ac_p = convert_pattern env arg_prefix p in
        let* _, ac_expr = convert_expr env expr in
        return (((ac_p, ac_expr) : case) :: acc))
    in
    let ac_cases = List.rev ac_cases in
    return (env, Match (ac_expr, ac_cases))
;;

let convert_prog (prog : prog) : prog t =
  let env = NameEnv.empty in
  let* _, ac_prog =
    fold_list prog ~init:(env, []) ~f:(fun (env, acc) expr ->
      let* env, ac_expr = convert_expr env expr in
      return (env, ac_expr :: acc))
  in
  return @@ List.rev ac_prog
;;

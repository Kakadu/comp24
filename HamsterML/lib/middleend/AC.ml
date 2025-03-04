open Ast
open Base
module Format = Stdlib.Format
open Utils.R

module NameSet = struct
  include Utils.NameSet
end

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

  let sub (env1 : t) (env2 : t) : t =
    Map.fold env1 ~init:empty ~f:(fun ~key ~data acc ->
      match Map.find env2 key with
      | None -> extend (key, data) acc
      | Some _ -> acc)
  ;;
end

let rec convert_pattern (env : NameEnv.t) prefix = function
  | (Const _ | Wildcard | Operation (Unary _)) as orig -> return (env, orig)
  | Var id ->
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

let convert_arg_pattern (env : NameEnv.t) (arg : pattern) (name_prefix : string) =
  match arg with
  | Var id ->
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
      let* env, ac_arg = convert_arg_pattern env arg arg_prefix in
      return (env, ac_arg :: acc))
  in
  return (env, List.rev ac_args)
;;

let collect_mutual_names (env : NameEnv.t) (binds : bind list) =
  fold_list binds ~init:env ~f:(fun env (name, _, _) ->
    let* env, _ = convert_pattern env let_prefix name in
    return env)
;;

let rec convert_bind (env : NameEnv.t) ((name, args, body) : bind) =
  let* env, new_name = convert_pattern env let_prefix name in
  let* args_env, ac_args = collect_args env args in
  let* env, ac_body = convert_expr args_env body in
  let env = NameEnv.sub env args_env in
  return (env, ((new_name, ac_args, ac_body) : bind))

and convert_expr (env : NameEnv.t) = function
  | (EConst _ | EOperation _) as orig -> return (env, orig)
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
    let* env, ac_body = convert_expr args_env body in
    let env = NameEnv.sub args_env env in
    return (env, Fun (ac_args, ac_body))
  | Let (rec_flag, binds, scope) ->
    let* env =
      match rec_flag with
      | Recursive -> collect_mutual_names env binds
      | Nonrecursive -> return env
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
  | _ -> failwith "Illegal expression was encountered during Alpha Conversion"
;;

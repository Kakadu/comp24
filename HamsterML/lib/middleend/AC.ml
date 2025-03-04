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

let arg_prefix = "arg_"
let let_prefix = "var_"

let rec ac_pattern (env : NameEnv.t) = function
  | (Const _ | Wildcard | Operation (Unary _)) as orig -> return (env, orig)
  | Var id ->
    let* env, new_id = NameEnv.resolve_name env id arg_prefix in
    return (env, Var new_id)
  | Operation (Binary op) ->
    let op_id = BinOperator.to_string op in
    let* env, new_id = NameEnv.resolve_name env op_id arg_prefix in
    return (env, Var new_id)
  | Constraint (p, dt) ->
    let* env, ac_p = ac_pattern env p in
    return (env, Constraint (ac_p, dt))
  | List pts ->
    let* env, ac_pts =
      fold_list pts ~init:(env, []) ~f:(fun (env, acc) p ->
        let* env, ac_p = ac_pattern env p in
        return (env, ac_p :: acc))
    in
    let ac_pts = List.rev ac_pts in
    return (env, List ac_pts)
  | ListConcat (p1, p2) ->
    let* env, ac_p1 = ac_pattern env p1 in
    let* env, ac_p2 = ac_pattern env p2 in
    return (env, ListConcat (ac_p1, ac_p2))
  | Tuple (a, b, pts) ->
    let* env, ac_a = ac_pattern env a in
    let* env, ac_b = ac_pattern env b in
    let* env, ac_pts =
      fold_list pts ~init:(env, []) ~f:(fun (env, acc) p ->
        let* env, ac_p = ac_pattern env p in
        return (env, ac_p :: acc))
    in
    let ac_pts = List.rev ac_pts in
    return (env, Tuple (ac_a, ac_b, ac_pts))
;;

let ac_name_pattern (env : NameEnv.t) (arg : pattern) (name_prefix : string) =
  match arg with
  | Var id ->
    let* env, new_id = NameEnv.generate_new_name env id name_prefix in
    return (env, Var new_id)
  | Operation (Binary op) ->
    let op_id = BinOperator.to_string op in
    let* env, new_id = NameEnv.resolve_name env op_id name_prefix in
    return (env, Var new_id)
  | arg -> ac_pattern env arg
;;

let collect_args (env : NameEnv.t) (args : args) =
  let* env, ac_args =
    fold_list args ~init:(env, []) ~f:(fun (env, acc) arg ->
      let* env, ac_arg = ac_name_pattern env arg arg_prefix in
      return (env, ac_arg :: acc))
  in
  return (env, List.rev ac_args)
;;

let rec ac_bind (env : NameEnv.t) ((name, args, body) : bind) =
  let* env, new_name = ac_name_pattern env name let_prefix in
  let* args_env, ac_args = collect_args env args in
  let* env, ac_body = ac_expr args_env body in
  let env = NameEnv.sub env args_env in
  return (env, ((new_name, ac_args, ac_body) : bind))

and ac_expr (env : NameEnv.t) = function
  | (EConst _ | EOperation _) as orig -> return (env, orig)
  | EVar id ->
    (match NameEnv.find id env with
     | Some new_id -> return (env, EVar new_id)
     | None -> return (env, EVar id))
  | Application (l, r) ->
    let* env, ac_l = ac_expr env l in
    let* env, ac_r = ac_expr env r in
    return (env, Application (ac_l, ac_r))
  | Fun (args, body) ->
    let* args_env, ac_args = collect_args env args in
    let* env, ac_body = ac_expr args_env body in
    let env = NameEnv.sub args_env env in
    return (env, Fun (ac_args, ac_body))
  | _ -> failwith "Illegal expression was encountered during Alpha Conversion"
;;

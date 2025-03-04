open Ast
open Base
module Format = Stdlib.Format
open Utils.R

module NameEnv = struct
  include Utils.NameEnv

  let rec generate_name (env : t) (name : id) (prefix : string) =
    match find name env with
    | None ->
      let* fresh_num = fresh in
      let new_name = prefix ^ Int.to_string fresh_num in
      (match find new_name env with
       | None -> return (extend (name, new_name) env, new_name)
       | Some _ -> generate_name env name prefix)
    | Some new_name -> return (env, new_name)
  ;;
end

let rec ac_pattern (env : NameEnv.t) = function
  | (Const _ | Operation _ | Wildcard) as orig -> return (env, orig)
  | Var id ->
    let* env, new_id = NameEnv.generate_name env id "var_" in
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

let rec ac_expr (env : NameEnv.t) = function
  | (EConst _ | EOperation _) as orig -> return orig
  | EVar id ->
    (match NameEnv.find id env with
     | Some new_id -> return (EVar new_id)
     | None -> return (EVar id))
  | ETuple (a, b, tl) ->
    let* ac_a = ac_expr env a in
    let* ac_b = ac_expr env b in
    let* ac_tl = map_list tl ~f:(ac_expr env) in
    return (ETuple (ac_a, ac_b, ac_tl))
  | _ -> failwith "Illegal expression was encountered during Alpha Conversion"
;;

(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Pe_ast
open Common
open Common.MonadCounter
open Common.StrSet

let rec ac_expr env bindings = function
  | Pe_EUnit -> return Pe_EUnit
  | Pe_ENill -> return Pe_ENill
  | Pe_EConst _ as c -> return c
  | Pe_EIdentifier x as v ->
    (match StrMap.find bindings x with
     | Some x -> return @@ Pe_EIdentifier x
     | None -> return v)
  | Pe_ECons (h, t) ->
    let* h = ac_expr env bindings h in
    let* t = ac_expr env bindings t in
    return @@ Pe_ECons (h, t)
  | Pe_EApp (e1, e2) ->
    let* e1 = ac_expr env bindings e1 in
    let* e2 = ac_expr env bindings e2 in
    return @@ Pe_EApp (e1, e2)
  | Pe_EIf (e1, e2, e3) ->
    let* e1 = ac_expr env bindings e1 in
    let* e2 = ac_expr env bindings e2 in
    let* e3 = ac_expr env bindings e3 in
    return @@ Pe_EIf (e1, e2, e3)
  | Pe_EFun (args, body) ->
    let* args, env, bindings =
      fold_left
        args
        ~init:(return ([], env, bindings))
        ~f:(fun (names, env, bindings) name ->
          let* env, bindings, name = rename env bindings name in
          return (name :: names, env, bindings))
    in
    let args = List.rev args in
    let* body = ac_expr env bindings body in
    return @@ Pe_EFun (args, body)
  | Pe_ETuple el ->
    let* e_list = map el ~f:(ac_expr env bindings) in
    return @@ Pe_ETuple e_list
  | Pe_ELet (rec_flag, name, e1, e2) ->
    let* new_e1 = ac_expr env bindings e1 in
    let* env, bindings, new_name = rename env bindings name in
    let* new_e2 = ac_expr env bindings e2 in
    return @@ Pe_ELet (rec_flag, new_name, new_e1, new_e2)

and rename env binds name =
  if String.equal name "()"
  then return (env, binds, "()")
  else if find env name
  then
    let* fresh = fresh in
    let id = get_id fresh in
    return (add env id, StrMap.update binds name ~f:(fun _ -> id), id)
  else return (add env name, binds, name)
;;

let ac_declaration env bindings = function
  | Pe_Nonrec bindings_list ->
      let ids, exps = List.unzip bindings_list in
      let* ids, env, bindings =
          fold_left
              ids
              ~init:(return ([], env, bindings))
              ~f:(fun (ids, env, bindings) id ->
                  let* env, bindings, id = rename env bindings id in
                  return (id :: ids, env, bindings))
      in
      let ids = List.rev ids in
      let exps = List.map exps ~f:(ac_expr env bindings) in
      let* bindings_list =
          List.fold2_exn
              ids
              exps
              ~init:(return [])
              ~f:(fun acc name expr ->
                  let* acc = acc in
                  let* expr = expr in
                  return ((name, expr) :: acc))
      in
      let bindings_list = List.rev bindings_list in
      return (env, bindings, Pe_Nonrec bindings_list)
  | Pe_Rec bindings_list ->
      let ids, exps = List.unzip bindings_list in
      let* ids, env, bindings =
          fold_left
              ids
              ~init:(return ([], env, bindings))
              ~f:(fun (ids, env, bindings) id ->
                  let* env, bindings, id = rename env bindings id in
                  return (id :: ids, env, bindings))
      in
      let ids = List.rev ids in
      let exps = List.map exps ~f:(ac_expr env bindings) in
      let* bindings_list =
          List.fold2_exn
              ids
              exps
              ~init:(return [])
              ~f:(fun acc name expr ->
                  let* acc = acc in
                  let* expr = expr in
                  return ((name, expr) :: acc))
      in
      let bindings_list = List.rev bindings_list in
      return (env, bindings, Pe_Rec bindings_list)
;;
let ac_program program env =
  let rec helper env bindings = function
    | [] -> return []
    | hd :: tl ->
      let* env, bindings, ast = ac_declaration env bindings hd in
      let* rest = helper env bindings tl in
      return (ast :: rest)
  in
  helper env (Map.empty (module String)) program
;;

let run_alpha_conv bindings init prog =
  run (ac_program prog (of_list builtins)) bindings init
;;
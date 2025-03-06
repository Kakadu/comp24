(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

module Env = Map.Make (String)
open Reduced_ast

type empty

open
  Utils.Counter_monad.Make
    (Int)
    (struct
      type t = empty
    end)

let fresh_name name =
  let+ num = fresh in
  name ^ Int.to_string num
;;

let update_env name m =
  let rec helper name m =
    match Env.find_opt name m with
    | Some _ ->
      let* new_name = fresh_name name in
      helper new_name m
    | None -> return name
  in
  let+ new_name = helper name m in
  let m = Env.add new_name new_name m in
  new_name, Env.add name new_name m
;;

let find name env =
  match Env.find_opt name env with
  | Some name -> name
  | None -> name
;;

let rec alpha_expr env = function
  | RExp_let (name, v, exp) ->
    let* v = alpha_expr env v in
    let* name, env = update_env name env in
    let+ exp = alpha_expr env exp in
    RExp_let (name, v, exp)
  | RExp_let_rec (vals, exp) ->
    let* env =
      fold_left
        (fun env (name, _) ->
          let+ _, env = update_env name env in
          env)
        (return env)
        vals
    in
    let+ vals =
      map
        (fun (name, exp) ->
          let+ exp = alpha_expr env exp in
          find name env, exp)
        vals
    and+ exp = alpha_expr env exp in
    RExp_let_rec (vals, exp)
  | RExp_ident name -> return @@ RExp_ident (find name env)
  | RExp_fun (args, exp) ->
    let* env, rev_args =
      fold_left
        (fun (env, prev) name ->
          let+ _, env = update_env name env in
          env, name :: prev)
        (return (env, []))
        args
    in
    let+ exp = alpha_expr env exp in
    RExp_fun (List.rev rev_args, exp)
  | RExp_apply (l, r) ->
    let+ l = alpha_expr env l
    and+ r = alpha_expr env r in
    RExp_apply (l, r)
  | RExp_tuple l ->
    let+ res = map (alpha_expr env) l in
    RExp_tuple res
  | RExp_construct (name, Some e) ->
    let+ e = alpha_expr env e in
    RExp_construct (name, Some e)
  | RExp_if (i, t, e) ->
    let+ i = alpha_expr env i
    and+ t = alpha_expr env t
    and+ e =
      match e with
      | Some e ->
        let+ e = alpha_expr env e in
        Some e
      | _ as e -> return e
    in
    RExp_if (i, t, e)
  | _ as e -> return e
;;

let init_env =
  fold_left
    (fun env (_, _, name) ->
      let+ _, env = update_env name env in
      env)
    (return Env.empty)
    Utils.Predefined_ops.predefine_operators
;;

let alpha_conversion rast =
  let res =
    let* env = init_env in
    let+ _, res =
      fold_left
        (fun (env, prev) cur ->
          match cur with
          | RStr_eval e ->
            let+ e = alpha_expr env e in
            env, RStr_eval e :: prev
          | RStr_value (name, e) ->
            let* _, env = update_env name env in
            let+ e = alpha_expr env e in
            env, RStr_value (name, e) :: prev
          | RStr_value_rec vals ->
            let* env =
              fold_left
                (fun env (name, _) ->
                  let+ _, env = update_env name env in
                  env)
                (return env)
                vals
            in
            let+ vals =
              map
                (fun (name, exp) ->
                  let+ exp = alpha_expr env exp in
                  name, exp)
                vals
            in
            env, RStr_value_rec vals :: prev)
        (return (env, []))
        rast
    in
    List.rev res
  in
  match run res 0 with
  | Result.Ok s -> s
  | Result.Error _ -> []
;;

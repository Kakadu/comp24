(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Sast
open Base
open Utils
open Vars
open Types
open Sast

let env_to_str env =
  Map.to_alist env
  |> List.map ~f:(fun (k, v) -> k ^ ": " ^ set_to_string v)
  |> String.concat ~sep:", "
;;

let rec cc_expr globals env ?(apply = true) = function
  | SConst c -> s_const c
  | SVar id ->
    (match Map.find env id with
     | None -> s_var id
     | Some fvs ->
       fvs |> Set.to_list |> List.map ~f:s_var |> List.fold ~init:(s_var id) ~f:s_app)
  | SApp (l, r) -> s_app (cc_expr globals env l) (cc_expr globals env r)
  | SIfElse (c, t, e) ->
    s_if_else (cc_expr globals env c) (cc_expr globals env t) (cc_expr globals env e)
  | SFun (p, ps, body) as func ->
    let pat = p :: ps in
    let fvs = Set.diff (free_vars_sexpr func) globals |> Set.to_list in
    let body' = cc_expr globals env body in
    (match fvs with
     | [] -> s_fun p ps body'
     | hd :: tl ->
       let p, ps = hd, List.append tl pat in
       let closure_fun = s_fun p ps body' in
       if apply
       then fvs |> List.map ~f:s_var |> List.fold ~init:closure_fun ~f:s_app
       else closure_fun)
  | SLetIn (def, exp) ->
    let def', env', _ = cc_def globals env def ~apply:false in
    let exp' = cc_expr globals env' exp in
    s_let_in def' exp'
  | STuple (x1, x2, xs) ->
    s_tuple
      (cc_expr globals env x1)
      (cc_expr globals env x2)
      (List.map xs ~f:(cc_expr globals env))
  | SList xs -> List.map xs ~f:(cc_expr globals env) |> s_list

and cc_def globals env ?(apply = true) = function
  | SLet (flag, id, (SFun _ as func)) as def ->
    let fvs = Set.diff (free_vars_sdef def) globals in
    let env' = Map.set env ~key:id ~data:fvs in
    let globals' = Set.add globals id in
    let func = cc_expr globals' env' func ~apply in
    s_let_flag flag id func, env', globals'
  | SLet (flag, id, exp) ->
    let exp' = cc_expr globals env exp in
    s_let_flag flag id exp', env, globals
;;

let default_globals =
  let open Std in
  stdlib @ runtime |> List.map ~f:(fun x -> x.name) |> Set.Poly.of_list
;;

let closure_conversion ?(globals = default_globals) (program : sprogram) =
  let program = Simplify.simplify program in
  let env = Map.Poly.empty in
  let helper prog =
    List.fold_map prog ~init:globals ~f:(fun globals ->
        function
        | SLet (_, id, _) as def ->
          let def', _, _ = cc_def globals env def in
          let globals' = Set.add globals id in
          globals', def')
  in
  helper program |> snd
;;

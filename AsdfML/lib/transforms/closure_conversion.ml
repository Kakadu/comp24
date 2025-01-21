(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Tast
open Ast
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

let pat_list_to_id_list =
  List.map ~f:(function
    | PIdent id -> id
    | PWild -> "_"
    | _ -> failwith "not id")
;;

let rec cc_expr globals env ?(apply = true) = function
  | TEConst (_, c) -> s_const c
  | TEVar (_, id) ->
    (match Map.find env id with
     | None -> s_var id
     | Some fvs ->
       dbg "FV in %s: %s\n" id (set_to_string fvs);
       fvs |> Set.to_list |> List.map ~f:s_var |> List.fold ~init:(s_var id) ~f:s_app)
  | TEApp (_, l, r) -> s_app (cc_expr globals env l) (cc_expr globals env r)
  | TEIfElse (_, c, t, e) ->
    s_if_else (cc_expr globals env c) (cc_expr globals env t) (cc_expr globals env e)
  | TEFun (_, pat, body) as func ->
    let fvs = Set.diff (free_vars_texpr func) globals |> Set.to_list in
    dbg "FVs %s in fun\n%a\n" (list_to_string fvs) Tast.pp_texpr func;
    (* dbg "GLOBALS: %s\n" (set_to_string globals); *)
    let body' = cc_expr globals env body in
    (match fvs with
     | [] -> s_fun (pat_list_to_id_list pat) body'
     | _ ->
       dbg
         "Creating (apply:%b) closure with FVs %s\nand body %a\n"
         apply
         (list_to_string fvs)
         Sast.pp_sexpr
         body';
       let pat = fvs |> List.map ~f:p_ident |> (Fn.flip List.append) pat in
       let closure_fun = s_fun (pat_list_to_id_list pat) body' in
       if apply
       then fvs |> List.map ~f:s_var |> List.fold ~init:closure_fun ~f:s_app
       else closure_fun)
  | TELetIn (_, def, exp) ->
    (* let def', env', globals' = cc_def globals env def ~apply:false in
       let exp' = cc_expr globals' env' exp in *)
    let def', env', _ = cc_def globals env def ~apply:false in
    let exp' = cc_expr globals env' exp in
    s_let_in def' exp'
  | TETuple (_, xs) -> List.map xs ~f:(cc_expr globals env) |> s_tuple
  | TEList (_, xs) -> List.map xs ~f:(cc_expr globals env) |> s_list
  | TEMatch _ -> failwith "removed by now"

and cc_def globals env ?(apply = true) = function
  | TDLet (TArrow _, flag, PIdent id, func) as def ->
    let fvs = Set.diff (free_vars_tdef def) globals in
    let env' = Map.set env ~key:id ~data:fvs in
    let globals' = Set.add globals id in
    let func = cc_expr globals' env' func ~apply in
    s_let true flag id func, env', globals'
  | TDLet (_, flag, PIdent id, exp) ->
    let exp' = cc_expr globals env exp in
    s_let false flag id exp', env, globals
  | TDLet (_, _, PWild, exp) -> 
    let exp' = cc_expr globals env exp in
    s_let false NonRec "_" exp', env, globals
  | _ -> failwith "cc_def: not implemented"
;;

let default_globals =
  let open Std in
  stdlib @ runtime |> List.map ~f:(fun x -> x.name) |> Set.Poly.of_list
;;

let closure_conversion ?(globals = default_globals) (program : tdefinition list) =
  let program = Simplify.simplify program in
  let env = Map.Poly.empty in
  let helper prog =
    List.fold_map prog ~init:globals ~f:(fun globals ->
        function
        | TDLet (_, _, pat, _) as def ->
          let def', _, _ = cc_def globals env def in
          let globals' = Set.union globals (bound_vars_pat pat) in
          globals', def')
  in
  helper program |> snd
;;

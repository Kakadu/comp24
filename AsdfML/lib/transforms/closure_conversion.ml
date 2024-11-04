(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Base
open Utils
open Vars

(* TODO: clean debug prints *)


let env_to_str env =
  Map.to_alist env
  |> List.map ~f:(fun (k, v) -> k ^ ": " ^ set_to_string v)
  |> String.concat ~sep:", "
;;

let rec cc_expr globals env = function
  | EConst _ as c -> c
  | EVar id as var ->
    (match Map.find env id with
     | None -> var
     | Some fvs ->
       dbg "FV in %s: %s\n" id (set_to_string fvs);
       fvs |> Set.to_list |> List.map ~f:e_var |> List.fold ~init:var ~f:e_app)
  | EApp (l, r) -> e_app (cc_expr globals env l) (cc_expr globals env r)
  | EIfElse (c, t, e) ->
    e_if_else (cc_expr globals env c) (cc_expr globals env t) (cc_expr globals env e)
  | EFun (pat, body) as func ->
    let fvs = Set.diff (free_vars_expr func) globals |> Set.to_list in
    dbg "FVs %s in fun\n%a\n" (list_to_string fvs) Pp_ast.pp_expr func;
    let body' = cc_expr globals env body in
    (match fvs with
     | [] -> func
     | _ ->
       dbg
         "Creating closure with FVs %s\nand body %a\n"
         (list_to_string fvs)
         Pp_ast.pp_expr
         body';
       let pat = fvs |> List.map ~f:p_ident |> (Fn.flip List.append) pat in
       let closure_fun = e_fun pat body' in
       fvs |> List.map ~f:e_var |> List.fold ~init:closure_fun ~f:e_app)
  | ELetIn (def, exp) ->
    let def', env', globals' = cc_def globals env def in
    let exp' = cc_expr globals' env' exp in
    e_let_in def' exp'
  | ETuple xs -> List.map xs ~f:(cc_expr globals env) |> e_tuple
  | EList xs -> List.map xs ~f:(cc_expr globals env) |> e_list
  | EMatch (exp, cases) ->
    let exp' = cc_expr globals env exp in
    let cases' = List.map cases ~f:(fun (p, e) -> p, cc_expr globals env e) in
    e_match exp' cases'

and cc_def globals env = function
  (* TODO: other patterns *)
  | DLet (flag, (PIdent id as pat), (EFun (fpat, body) as func)) as def ->
    let fvs = Set.diff (free_vars_def def) globals in
    let fpat' = fvs |> Set.to_list |> List.map ~f:p_ident |> (Fn.flip List.append) fpat in
    let env' = Map.set env ~key:id ~data:fvs in
    let func = e_fun fpat' (cc_expr globals env' body) in
    let globals' = Set.add globals id in
    d_let_flag flag pat func, env', globals'
  | DLet (flag, (PIdent id as pat), exp) as def ->
    let exp' = cc_expr globals env exp in
    d_let_flag flag pat exp', env, globals
  | _ -> failwith "cc_def not implemented"
;;

let default_globals =
  let open Std in
  Std.stdlib |> List.map ~f:(fun x -> x.name) |> Set.Poly.of_list
;;

let closure_conversion ?(globals = default_globals) program =
  let program = Simplify.simplify program in
  let env = Map.Poly.empty in
  let helper prog =
    List.fold_map prog ~init:globals ~f:(fun globals ->
        function
        | DLet (NonRec, pat, _) as def ->
          let def', _, _ = cc_def globals env def in
          let globals' = Set.union globals (bound_vars_pat pat) in
          globals', def'
        | DLet (Rec, pat, _) as def ->
          let globals' = Set.union globals (bound_vars_pat pat) in
          let def', _, _ = cc_def globals' env def in
          globals', def')
  in
  helper program |> snd
;;

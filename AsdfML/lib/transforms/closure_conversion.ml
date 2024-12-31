(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Tast
open Ast
open Base
open Utils
open Vars

let env_to_str env =
  Map.to_alist env
  |> List.map ~f:(fun (k, v) -> k ^ ": " ^ set_to_string v)
  |> String.concat ~sep:", "
;;

let rec cc_expr globals env ?(apply = true) = function
  | TEConst _ as c -> c
  | TEVar (_, id) as var ->
    (match Map.find env id with
     | None -> var
     | Some fvs ->
       dbg "FV in %s: %s\n" id (set_to_string fvs);
       fvs
       |> Set.to_list
       |> List.map ~f:(te_var (TVar 0))
       |> List.fold ~init:var ~f:(te_app (TVar 0)))
  | TEApp (ty, l, r) -> te_app ty (cc_expr globals env l) (cc_expr globals env r)
  | TEIfElse (ty, c, t, e) ->
    te_if_else ty (cc_expr globals env c) (cc_expr globals env t) (cc_expr globals env e)
  | TEFun (ty, pat, body) as func ->
    let fvs = Set.diff (free_vars_texpr func) globals |> Set.to_list in
    dbg "FVs %s in fun\n%a\n" (list_to_string fvs) Tast.pp_texpr func;
    let body' = cc_expr globals env body in
    (match fvs with
     | [] -> te_fun ty pat body'
     | _ ->
       dbg
         "Creating (apply:%b) closure with FVs %s\nand body %a\n"
         apply
         (list_to_string fvs)
         Tast.pp_texpr
         body';
       let pat = fvs |> List.map ~f:p_ident |> (Fn.flip List.append) pat in
       let closure_fun = te_fun ty pat body' in
       if apply
       then
         fvs
         |> List.map ~f:(te_var (TVar 0))
         |> List.fold ~init:closure_fun ~f:(te_app (TVar 0))
       else closure_fun)
  | TELetIn (ty, def, exp) ->
    let def', env', globals' = cc_def globals env def ~apply:false in
    let exp' = cc_expr globals' env' exp in
    te_let_in ty def' exp'
  | TETuple (ty, xs) -> List.map xs ~f:(cc_expr globals env) |> te_tuple ty
  | TEList (ty, xs) -> List.map xs ~f:(cc_expr globals env) |> te_list ty
  (* 
  TODO: SHOULD BE REMOVED BY NOW
  | TEMatch (ty, exp, cases) ->
    let exp' = cc_expr globals env exp in
    let cases' = List.map cases ~f:(fun (p, e) -> p, cc_expr globals env e) in
    e_match exp' cases' *)
  | _ -> failwith "cc_expr not implemented"

and cc_def globals env ?(apply = true) = function
  | TDLet ((TArrow _ as ty), (_ as flag), (PIdent id as pat), func) as def ->
    let fvs = Set.diff (free_vars_tdef def) globals in
    let env' = Map.set env ~key:id ~data:fvs in
    let globals' = Set.add globals id in
    let func = cc_expr globals' env' func ~apply in
    td_let_flag flag ty pat func, env', globals'
  | TDLet (ty, flag, (PIdent _ as pat), exp) ->
    let exp' = cc_expr globals env exp in
    td_let_flag flag ty pat exp', env, globals
  | _ -> failwith "cc_def not implemented"
;;

let default_globals =
  let open Std in
  Std.stdlib |> List.map ~f:(fun x -> x.name) |> Set.Poly.of_list
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

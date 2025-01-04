(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Ast
open Sast
open Utils
open Cf_ast
open State.IntStateM
open State.IntStateM.Syntax

let fresh_id name =
  let* fresh = fresh in
  let fresh = string_of_int fresh in
  return (if String.equal name "" then "`ll_" ^ fresh else "`" ^ name ^ "_" ^ fresh)
;;

let rec ll_expr env lift ?(name = None) = function
  | SConst c -> return (cf_const c, lift)
  | SVar id ->
    let id =
      match Map.find env id with
      | None -> cf_var id
      | Some new_id -> cf_var new_id
    in
    return (id, lift)
  | SApp (l, r) ->
    let* l, lift = ll_expr env lift l in
    let* r, lift = ll_expr env lift r in
    return (cf_app l r, lift)
  | SIfElse (i, t, e) ->
    let* i, lift = ll_expr env lift i in
    let* t, lift = ll_expr env lift t in
    let* e, lift = ll_expr env lift e in
    return (cf_if_else i t e, lift)
  | SFun (args, exp) ->
    let* id =
      match name with
      | Some id -> return id
      | None -> fresh_id ""
    in
    let* exp, lift = ll_expr env lift exp in
    return (cf_var id, cf_def id args exp :: lift)
  | SLetIn ((SLet (is_fun, is_rec, id, body) as def), exp) ->
    (* TODO: useless function defs *)
    let* def, lift, _ = ll_def env lift def in
    let id, body =
      match def with
      | CFLet (id, _, body) -> id, body
    in
    let env = Map.remove env id in
    let* exp, lift = ll_expr env lift exp ~name in
    return (cf_let_in id body exp, lift)
  | (STuple xs | SList xs) as exp ->
    let[@warning "-8"] constr =
      match exp with
      | STuple _ -> cf_tuple
      | SList _ -> cf_list
    in
    List.fold
      xs
      ~init:(return ([], lift))
      ~f:(fun acc x ->
        let* acc, lift = acc in
        let* x, lift = ll_expr env lift x in
        return (x :: acc, lift))
    >>| fun (xs, lift) -> constr (List.rev xs), lift

and ll_def env lift = function
  (* TODO:
     let f x = ll_f x
     simplify?
     ~name
     lifts in `let test = fun ... -> ...`
  *)
  (* | SLet (true, is_rec, id, exp) ->
     let* new_id = fresh_id id in
     let env = Map.set env ~key:id ~data:new_id in
     let* exp, lift = ll_expr env lift exp ~name:(Some new_id) in
     return (cf_def id [] exp, lift, env) *)
  | SLet (_, is_rec, id, exp) ->
    (* TODO: new_id ?? *)
    let* new_id = fresh_id id in
    let env = Map.set env ~key:id ~data:new_id in
    let* exp, lift = ll_expr env lift exp ~name:(Some new_id) in
    return (cf_def id [] exp, lift, env)
;;

let ll_program prog =
  let empty = Map.empty (module String) in
  List.fold prog ~init:(return []) ~f:(fun acc def ->
    let* acc = acc in
    let* def, lift, _ = ll_def empty [] def in
    return ((def :: lift) @ acc))
  >>| List.rev
;;

let lambda_lifting program = run (ll_program program)

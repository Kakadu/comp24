(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Ast
open Sast
open Utils
open Cf_ast
open State.IntStateM
open State.IntStateM.Syntax

(* TODO:
   - lifts in `let test = fun ... -> ...`
*)

let rec ll_expr env lift ?(name = None) =
  let name_or_new () =
    match name with
    | Some x -> return x
    | None ->
      let* fresh = fresh in
      return ("'ll_" ^ string_of_int fresh)
  in
  function
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
    let* name = name_or_new () in
    let* exp, lift = ll_expr env lift exp in
    return (cf_var name, cf_def name args exp :: lift)
  | SLetIn (SLet (is_rec, id, body), exp) ->
    (* TODO: flag *)
    let* body, lift = ll_expr env lift body in
    let* exp, lift = ll_expr env lift exp in
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
  | SLet (Rec, id, (SFun (args, _) as exp)) ->
    let* fresh = fresh in
    let name = Format.sprintf "'ll_%d_%s" fresh id in
    let env = Map.set env ~key:id ~data:name in
    let* exp, lift = ll_expr env lift exp ~name:(Some name) in
    return (cf_def name args exp, lift)
  | SLet (_, id, exp) ->
    let* exp, lift = ll_expr env lift exp in
    return (cf_def id [] exp, lift)
;;

let ll_program prog =
  let empty = Map.empty (module String) in
  List.fold prog ~init:(return []) ~f:(fun acc def ->
    let* acc = acc in
    let* def, lift = ll_def empty [] def in
    return ((def :: lift) @ acc))
  >>| List.rev
;;

let lambda_lifting program = run (ll_program program)

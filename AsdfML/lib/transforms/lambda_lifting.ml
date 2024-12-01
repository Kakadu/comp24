(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Ast
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
  | EConst c -> return (cf_const c, lift)
  | EVar id ->
    let id =
      match Map.find env id with
      | None -> cf_var id
      | Some new_id -> cf_var new_id
    in
    return (id, lift)
  | EApp (l, r) ->
    let* l, lift = ll_expr env lift l in
    let* r, lift = ll_expr env lift r in
    return (cf_app l r, lift)
  | EIfElse (i, t, e) ->
    let* i, lift = ll_expr env lift i in
    let* t, lift = ll_expr env lift t in
    let* e, lift = ll_expr env lift e in
    return (cf_if_else i t e, lift)
  | EFun (pat, exp) ->
    let* name = name_or_new () in
    let args =
      List.map pat ~f:(function
        | PIdent id -> id
        | PWild -> "_"
        | _ -> assert false)
    in
    let* exp, lift = ll_expr env lift exp in
    return (cf_var name, cf_def name (cf_fun args exp) :: lift)
  | ELetIn (def, exp) ->
    let* def, lift = ll_def env lift def in
    let* exp, lift = ll_expr env lift exp in
    return (cf_let_in def exp, lift)
  | ETuple xs ->
    List.fold
      xs
      ~init:(return ([], lift))
      ~f:(fun acc x ->
        let* acc, lift = acc in
        let* x, lift = ll_expr env lift x in
        return (x :: acc, lift))
    >>| fun (xs, lift) -> cf_tuple (List.rev xs), lift
  | EList xs ->
    List.fold
      xs
      ~init:(return ([], lift))
      ~f:(fun acc x ->
        let* acc, lift = acc in
        let* x, lift = ll_expr env lift x in
        return (x :: acc, lift))
    >>| fun (xs, lift) -> cf_list (List.rev xs), lift
  | EMatch (exp, cases) ->
    let* exp, lift = ll_expr env lift exp in
    List.fold
      cases
      ~init:(return ([], lift))
      ~f:(fun acc (pat, exp) ->
        let* acc, lift = acc in
        let* exp, lift = ll_expr env lift exp in
        return ((pat, exp) :: acc, lift))
    >>| fun (cases, lift) -> cf_match exp (List.rev cases), lift

and ll_def env lift = function
  | DLet (Rec, PIdent id, (EFun _ as exp)) ->
    let* fresh = fresh in
    let name = Format.sprintf "'ll_%d_%s" fresh id in
    let env = Map.set env ~key:id ~data:name in
    let* exp, lift = ll_expr env lift exp ~name:(Some name) in
    return (cf_def id exp, lift)
  | DLet (is_rec, PIdent id, exp) ->
    let* exp, lift = ll_expr env lift exp in
    return (cf_def id exp, lift)
  | _ -> failwith "todo ll_decl"
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

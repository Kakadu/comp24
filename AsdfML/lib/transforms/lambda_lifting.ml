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

let remove_patterns pat exp =
  let generate_unique_arg_name used =
    let start_number = Set.length used in
    let rec find_unique current_number =
      let current_name = "arg_" ^ string_of_int current_number in
      match Set.find used ~f:(String.equal current_name) with
      | None -> current_name
      | _ -> find_unique (current_number + 1)
    in
    find_unique start_number
  in
  let rec helper used current_expr args_list = function
    | head :: tail ->
      (match head with
       | PIdent id -> helper (Set.add used id) current_expr (id :: args_list) tail
       | _ ->
         let arg_name = generate_unique_arg_name used in
         helper
           (Set.add used arg_name)
           (e_match (e_var arg_name) [ head, current_expr ])
           (arg_name :: args_list)
           tail)
    | _ -> List.rev args_list, current_expr
  in
  let used = Vars.free_vars_expr exp in
  helper used exp [] pat
;;

let rec ll_expr env lift = function
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
    let* fresh = fresh in
    let name = "'ll_" ^ string_of_int fresh in
    let args, body = remove_patterns pat exp in
    let* body, lift = ll_expr env lift body in
    return (cf_var name, cf_def name (cf_fun args body) :: lift)
  | ELetIn (def, exp) ->
    let* (def : cf_definition), lift = ll_def env lift def in
    let* exp, lift = ll_expr env lift exp in
    return (cf_let_in def exp, lift)
  | ETuple xs -> failwith "todo ll_expr tuple"
  | EList xs -> failwith "todo ll_expr list"
  | EMatch (e, c) -> failwith "todo ll_expr match"

and ll_def env lift = function
  | DLet (Rec, PIdent id, (EFun _ as exp)) ->
    let* fresh = fresh in
    (* TODO: +1 *)
    let fresh = "'ll_" ^ string_of_int (fresh + 1) in
    let env = Map.set env ~key:id ~data:fresh in
    let* body, lift = ll_expr env lift exp in
    return (cf_def id body, lift)
  | DLet (is_rec, PIdent id, exp) ->
    let* body, lift = ll_expr env lift exp in
    return (cf_def id body, lift)
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

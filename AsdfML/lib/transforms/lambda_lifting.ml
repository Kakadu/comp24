(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Ast
open Cf_ast

open State.IntStateM
open State.IntStateM.Syntax

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
      match Base.Map.find env id with
      | None -> cf_var id
      | Some new_id -> cf_var new_id
    in
    return (id, lift)
  | EApp (l, r) ->
    let* l, lift = ll_expr env lift l in
    let* r, lift = ll_expr env lift r in
    return (cf_app l r, lift)
  | EIfElse (i, t, e) -> failwith "todo ll_expr ifelse"
  | EFun (pat, exp) ->
    let* fresh_var = fresh in
    let name = "'ll_" ^ string_of_int fresh_var in
    let args, body = remove_patterns pat exp in
    let* body, lifted = ll_expr env lift body in
    return (cf_var name, cf_def name (cf_fun args body) :: lifted)
  | ELetIn (DLet (r, p, e1), e2) -> failwith "todo ll_expr eletin"
  | ETuple xs -> failwith "todo ll_expr"
  | EList xs -> failwith "todo ll_expr"
  | EMatch (e, c) -> failwith "todo ll_expr ematch"

and ll_decl env lift = function
  | DLet (_, PIdent id, exp) ->
    let* body, lift = ll_expr env lift exp in
    return (cf_def id body, lift)
  | _ -> failwith "todo ll_decl"
;;

let rec ll_program = function
  | [] -> return []
  | DLet (_, PIdent id, body) :: tl ->
    let* body, lift = ll_expr (Base.Map.empty (module Base.String)) [] body in
    let* tail = ll_program tl in
    return (tail @ (cf_def id body :: lift))
  | _ -> failwith "todo ll_program"
;;

let lambda_lifting program = run (ll_program program)

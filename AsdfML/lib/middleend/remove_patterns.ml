(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Ast
open Sast
open Types
open Utils
open Vars
open State.IntStateM
open State.IntStateM.Syntax

(* Replaces complex patterns with explicit pattern-matching *)

(* TODO: constant pattens? *)

let list_field lst idx = e_app (e_app (e_var "`list_field") lst) (e_const (CInt idx))
let list_hd lst = e_app (e_var "`list_hd") lst
let list_tl lst = e_app (e_var "`list_tl") lst

let tuple_field lst idx =
  e_app (e_app (e_var "`get_tuple_field") lst) (e_const (CInt idx))
;;

let is_simple = function
  | PIdent _ | PWild -> true
  | _ -> false
;;

let remove_argument_patterns body pattern_list =
  let generate_arg_name used_names =
    let rec find_unique () =
      let* cnt = fresh_postfix in
      let current_name = "`arg" ^ cnt in
      match Set.find used_names ~f:(String.equal current_name) with
      | None -> return current_name
      | _ -> find_unique ()
    in
    run (find_unique ())
  in
  let rec helper used_names current_expr args_list = function
    | head :: tail ->
      (match head with
       | PIdent id -> helper (Set.add used_names id) current_expr (id :: args_list) tail
       | PWild -> helper used_names current_expr ("_" :: args_list) tail
       | _ ->
         let arg_name = generate_arg_name used_names in
         helper
           (Set.add used_names arg_name)
           (e_match (e_var arg_name) [ head, current_expr ])
           (arg_name :: args_list)
           tail)
    | _ -> List.rev args_list, current_expr
  in
  let used_names = vars_expr body in
  let ids, exp = helper used_names body [] pattern_list in
  List.map ids ~f:p_ident, exp
;;

let remove_patterns =
  let rec helper_expr = function
    | EConst _ as c -> c
    | EVar _ as v -> v
    | EApp (l, r) -> e_app (helper_expr l) (helper_expr r)
    | EIfElse (i, t, e) -> e_if_else (helper_expr i) (helper_expr t) (helper_expr e)
    | EFun (p, ps, e) ->
      let e = helper_expr e in
      let ps, e = remove_argument_patterns e (p :: ps) in
      (* TODO: *)
      let p, ps =
        match ps with
        | hd :: tl -> hd, tl
        | _ -> failwith "no args"
      in
      e_fun p ps e
    | ELetIn (def, body) ->
      let body = helper_expr body in
      (match def with
       | DLet (_, pat, exp) ->
         if is_simple pat
         then e_let_in (helper_def def |> List.hd_exn) body
         else e_match (helper_expr exp) [ pat, body ])
    | ETuple (x1, x2, xs) ->
      e_tuple (helper_expr x1) (helper_expr x2) (List.map xs ~f:helper_expr)
    | EList xs -> e_list (List.map xs ~f:helper_expr)
    | EMatch (e, c) ->
      e_match (helper_expr e) (List.map c ~f:(fun (p, e) -> p, helper_expr e))
  and helper_def = function
    | DLet (r, p, e) when is_simple p -> [ d_let_flag r p (helper_expr e) ]
    | DLet (r, PTuple (x1, x2, xs), e) ->
      let xs = x1 :: x2 :: xs in
      let temp_var = "`temp_tuple" in
      let temp_def = d_let_flag r (p_ident temp_var) (helper_expr e) in
      let assigns =
        List.mapi xs ~f:(fun i x -> d_let x (tuple_field (e_var temp_var) i))
      in
      temp_def :: assigns
    | DLet (r, (PList xs as pat), e) ->
      let temp_var = "`temp_list" in
      let e' = helper_expr e in
      let temp_def = d_let_flag r (p_ident temp_var) (e_match e' [ pat, e' ]) in
      let assigns =
        List.mapi xs ~f:(fun i x -> d_let x (list_field (e_var temp_var) i))
      in
      temp_def :: assigns
    | DLet (r, PCons (hd, tl), e) ->
      let temp_var = "`temp_list" in
      let temp_def = d_let_flag r (p_ident temp_var) (helper_expr e) in
      let hd_assign = d_let hd (list_hd (e_var temp_var)) in
      let tl_assign = d_let tl (list_tl (e_var temp_var)) in
      [ temp_def; hd_assign; tl_assign ]
    | _ ->
      (* TODO: recursive assignment for complex patterns *)
      failwith "complex patterns in top level let bindings are not supported"
  in
  fun (x : definition list) -> List.map x ~f:helper_def |> List.concat
;;

(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Ast
open Tast
open Types
open Utils
open Vars
open State.IntStateM
open State.IntStateM.Syntax

(* TODO: constant pattens? *)

let is_simple = function
  | PIdent _ | PWild -> true
  | _ -> false
;;

let eliminate_arg_patterns body pattern_list =
  let generate_arg_name used_names =
    let rec find_unique () =
      let* cnt = fresh in
      let current_name = "`arg_" ^ string_of_int cnt in
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
           (te_match
              (texpr_type current_expr)
              (te_var dummy_ty arg_name)
              [ head, current_expr ])
           (arg_name :: args_list)
           tail)
    | _ -> List.rev args_list, current_expr
  in
  let used_names = vars_texpr body in
  let ids, exp = helper used_names body [] pattern_list in
  List.map ids ~f:p_ident, exp
;;

let remove_patterns =
  let rec helper_expr = function
    | TEConst _ as c -> c
    | TEVar _ as v -> v
    | TEApp (t, l, r) -> te_app t (helper_expr l) (helper_expr r)
    | TEIfElse (ty, i, t, e) ->
      te_if_else ty (helper_expr i) (helper_expr t) (helper_expr e)
    | TEFun (t, p, e) ->
      let e = helper_expr e in
      let p, e = eliminate_arg_patterns e p in
      (* dbg "fun: %a\n" pp_texpr f;
         dbg "pat: %s\n" (p |> List.map ~f:(Format.asprintf "%a" pp_pattern) |> String.concat); *)
      te_fun t p e
    | TELetIn (t, def, body) ->
      let body = helper_expr body in
      (match def with
       | TDLet (_, _, pat, exp) ->
         if is_simple pat
         then te_let_in t (helper_def def |> List.hd_exn) body
         else te_match t (helper_expr exp) [ pat, body ])
    | TETuple (t, x1, x2, xs) ->
      te_tuple t (helper_expr x1) (helper_expr x2) (List.map xs ~f:helper_expr)
    | TEList (t, xs) -> te_list t (List.map xs ~f:helper_expr)
    | TEMatch (t, e, c) ->
      te_match t (helper_expr e) (List.map c ~f:(fun (p, e) -> p, helper_expr e))
  and helper_def = function
    | TDLet (t, r, p, e) when is_simple p -> [ td_let_flag r t p (helper_expr e) ]
    | TDLet (t, r, PTuple (x1, x2, xs), e) ->
      let xs = x1 :: x2 :: xs in
      let temp_var = p_ident "`temp_tuple" in
      let temp_def = td_let_flag r t temp_var (helper_expr e) in
      let assigns =
        List.mapi xs ~f:(fun i x ->
          td_let dummy_ty x (Remove_match.tuple_field (te_var dummy_ty "`temp_tuple") i))
      in
      temp_def :: assigns
    | TDLet (t, r, (PList xs as pat), e) ->
      let temp_var = p_ident "`temp_list" in
      let e' = helper_expr e in
      let temp_def = td_let_flag r t temp_var (te_match t e' [ pat, e' ]) in
      let assigns =
        List.mapi xs ~f:(fun i x ->
          td_let dummy_ty x (Remove_match.list_field (te_var dummy_ty "`temp_list") i))
      in
      temp_def :: assigns
    | TDLet (t, r, PCons (hd, tl), e) ->
      let temp_var = p_ident "`temp_list" in
      let temp_def = td_let_flag r t temp_var (helper_expr e) in
      let hd_assign =
        td_let dummy_ty hd (Remove_match.list_hd (te_var dummy_ty "`temp_list"))
      in
      let tl_assign =
        td_let dummy_ty tl (Remove_match.list_tl (te_var dummy_ty "`temp_list"))
      in
      [ temp_def; hd_assign; tl_assign ]
    | _ ->
      (* TODO: recursive assignment for complex patterns *)
      failwith "complex patterns in top level let bindings are not supported"
  in
  (* From here, a pattern in definition should be a single id/wildcard *)
  fun x -> List.map x ~f:helper_def |> List.concat
;;

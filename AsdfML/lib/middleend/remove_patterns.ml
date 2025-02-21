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

let remove_argument_patterns body pattern pattern_list =
  let generate_arg_name used_names =
    let rec find_unique () =
      let* current_name = fresh_prefix "`arg" in
      match Set.find used_names ~f:(String.equal current_name) with
      | None -> return current_name
      | _ -> find_unique ()
    in
    run (find_unique ())
  in
  let helper (used_names, current_expr) pattern =
    match pattern with
    | PIdent id -> Set.add used_names id, current_expr, pattern
    | PWild -> used_names, current_expr, pattern
    | _ ->
      let arg_name = generate_arg_name used_names in
      ( Set.add used_names arg_name
      , e_match (e_var arg_name) [ pattern, current_expr ]
      , p_ident arg_name )
  in
  let used_names = vars_expr body in
  let used_names, exp, pattern' = helper (used_names, body) pattern in
  let _, exp, pattern_list' =
    List.fold_right
      ~f:(fun x acc ->
        let used_names, exp, pattern_list = acc in
        let used_names, current_expr, pattern' = helper (used_names, exp) x in
        used_names, current_expr, pattern' :: pattern_list)
      ~init:(used_names, exp, [])
      pattern_list
  in
  pattern', pattern_list', exp
;;

let remove_patterns program =
  let rec helper_expr = function
    | EConst _ as c -> return c
    | EVar _ as v -> return v
    | EApp (l, r) ->
      let* l = helper_expr l in
      let* r = helper_expr r in
      return @@ e_app l r
    | EIfElse (i, t, e) ->
      let* i = helper_expr i in
      let* t = helper_expr t in
      let* e = helper_expr e in
      return @@ e_if_else i t e
    | EFun (p, ps, e) ->
      let* e = helper_expr e in
      let p, ps, e = remove_argument_patterns e p ps in
      return @@ e_fun p ps e
    | ELetIn (def, body) ->
      let* body = helper_expr body in
      (match def with
       | DLet (_, pat, exp) ->
         if is_simple pat
         then
           let* def = helper_def def in
           let def = List.hd_exn def in
           return @@ e_let_in def body
         else
           let* exp = helper_expr exp in
           return @@ e_match exp [ pat, body ])
    | ETuple (x1, x2, xs) ->
      let* x1 = helper_expr x1 in
      let* x2 = helper_expr x2 in
      let* xs =
        State.mfold_right xs ~init:[] ~f:(fun x acc ->
          helper_expr x >>| fun x -> x :: acc)
      in
      return @@ e_tuple x1 x2 xs
    | EList xs ->
      let* xs =
        State.mfold_right xs ~init:[] ~f:(fun x acc ->
          helper_expr x >>| fun x -> x :: acc)
      in
      return @@ e_list xs
    | EMatch (exp, cases) ->
      let* exp = helper_expr exp in
      let* cases =
        State.mfold_right cases ~init:[] ~f:(fun (pat, exp) acc ->
          let* exp = helper_expr exp in
          return ((pat, exp) :: acc))
      in
      return @@ e_match exp cases
  and helper_def : definition -> (var_id, definition list) State.StateM.t =
    let rec helper_complex exp = function
      | PTuple (x1, x2, xn) ->
        let xs = x1 :: x2 :: xn in
        let* temp_var = fresh_prefix "`temp_tuple" in
        let* exp = helper_expr exp in
        let temp_def = d_let (p_ident temp_var) exp in
        let* assigns =
          State.mfoldi_right xs ~init:[] ~f:(fun i acc x ->
            let* x = helper_complex (tuple_field (e_var temp_var) i) x in
            return @@ x @ acc)
        in
        return (temp_def :: assigns)
      | PList xs ->
        let* temp_var = fresh_prefix "`temp_list" in
        let* exp = helper_expr exp in
        let temp_def = d_let (p_ident temp_var) exp in
        let* assigns =
          State.mfoldi_right xs ~init:[] ~f:(fun i acc x ->
            let* x = helper_complex (list_field (e_var temp_var) i) x in
            return @@ x @ acc)
        in
        return (temp_def :: assigns)
      | PCons (hd, tl) ->
        let* temp_var = fresh_prefix "`temp_list" in
        let* exp = helper_expr exp in
        let temp_def = d_let (p_ident temp_var) exp in
        let hd_assign = d_let hd (list_hd (e_var temp_var)) in
        let tl_assign = d_let tl (list_tl (e_var temp_var)) in
        return [ temp_def; hd_assign; tl_assign ]
      | (PWild | PIdent _) as p ->
        let* e = helper_expr exp in
        return [ d_let p e ]
      | _ -> failwith "complex patterns in top level let bindings are not supported"
    in
    function
    | DLet (r, p, e) when is_simple p ->
      let* e = helper_expr e in
      return [ d_let_flag r p e ]
    | DLet (_, pat, e) ->
      (* Convert `let complex_pattern = expr`
         into `let temp_match = match expr with | complex_pattern -> ()`
         and deal with it later in 'remove_match' *)
      let* temp_match = fresh_prefix "`temp_match" in
      let match_exp =
        d_let
          (p_ident temp_match)
          (e_let_in
             (d_let (p_ident temp_match) e)
             (e_match (e_var temp_match) [ pat, e_var temp_match ]))
      in
      return @@ [ match_exp ]
  in
  let helper_program program =
    State.mfold program ~init:[] ~f:(fun acc def ->
      let* defs = helper_def def in
      return (defs @ acc))
    >>| List.rev
  in
  run (helper_program program)
;;

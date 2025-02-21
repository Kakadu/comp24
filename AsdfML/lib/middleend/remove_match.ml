(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Ast
open Sast
open Types
open Utils
open State.IntStateM
open State.IntStateM.Syntax

let true_ = SConst (CBool true)
let check_eq l r = SApp (SApp (SVar "( = )", l), r)
let panic () = SApp (SVar "panic", SConst CUnit)

let and_ l r =
  match l, r with
  | SConst (CBool true), _ | _, SConst (CBool false) -> r
  | _, SConst (CBool true) | SConst (CBool false), _ -> l
  | _ -> SApp (SApp (SVar "( && )", l), r)
;;

let not_ e = SApp (SVar "not", e)

let tuple_field lst idx =
  s_app (s_app (s_var "`get_tuple_field") lst) (s_const (CInt idx))
;;

let check_tuple_len tup len =
  check_eq (s_app (s_var "`tuple_len") tup) (s_const (CInt len))
;;

let check_list_len lst len = check_eq (s_app (s_var "`list_len") lst) (s_const (CInt len))
let list_field lst idx = s_app (s_app (s_var "`list_field") lst) (s_const (CInt idx))
let list_hd lst = s_app (s_var "`list_hd") lst
let list_tl lst = s_app (s_var "`list_tl") lst
let list_is_empty lst = s_app (s_var "`list_is_empty") lst

let pat_as_id = function
  | PIdent x -> x
  | PWild -> "_"
  | p -> failwith (Format.asprintf "remove_match: non-id pattern %a" Ast.pp_pattern p)
;;

let remove_match prog =
  let rec helper_expr = function
    | EConst c -> return (s_const c)
    | EVar v -> return (s_var v)
    | EApp (l, r) ->
      let* l = helper_expr l in
      let* r = helper_expr r in
      return (s_app l r)
    | EIfElse (i, t, e) ->
      let* i = helper_expr i in
      let* t = helper_expr t in
      let* e = helper_expr e in
      return (s_if_else i t e)
    | EFun (p, ps, e) ->
      let* e = helper_expr e in
      let p = pat_as_id p in
      let ps = List.map ps ~f:pat_as_id in
      return (s_fun p ps e)
    | ELetIn (d, e) ->
      let* d = helper_def d in
      let* e = helper_expr e in
      return (s_let_in d e)
    | ETuple (x1, x2, xs) ->
      let* x1 = helper_expr x1 in
      let* x2 = helper_expr x2 in
      let* xs =
        State.mfold_right xs ~init:[] ~f:(fun e acc ->
          let* e = helper_expr e in
          return (e :: acc))
      in
      return (s_tuple x1 x2 xs)
    | EList xs ->
      let* xs =
        State.mfold_right xs ~init:[] ~f:(fun e acc ->
          let* e = helper_expr e in
          return (e :: acc))
      in
      return (s_list xs)
    | EMatch (match_exp, cases) ->
      let rec bind_pat_vars match_exp pat action =
        match pat with
        | PIdent id -> s_let_in (s_let id match_exp) action |> return
        | PTuple (x1, x2, xs) ->
          let xs = x1 :: x2 :: xs in
          let* tuple_id = fresh_prefix "`tuple"  in
          let* body =
            State.mfoldi_right xs ~init:action ~f:(fun idx acc x ->
              bind_pat_vars (tuple_field (s_var tuple_id) idx) x acc)
          in
          s_let_in (s_let tuple_id match_exp) body |> return
        | PList xs ->
          let* list_id = fresh_prefix "`list" in
          let* body =
            State.mfoldi_right xs ~init:action ~f:(fun idx action x ->
              bind_pat_vars (list_field (s_var list_id) idx) x action)
          in
          s_let_in (s_let list_id match_exp) body |> return
        | PCons (hd, tl) ->
          let* tl = bind_pat_vars (list_tl match_exp) tl action in
          bind_pat_vars (list_hd match_exp) hd tl
        | _ -> return action
      in
      let rec case_matched match_exp = function
        | PConst (CInt _ as c) | PConst (CBool _ as c) -> check_eq match_exp (SConst c)
        | PConst CNil -> list_is_empty match_exp
        | PTuple (x1, x2, xs) ->
          let xs = x1 :: x2 :: xs in
          let check_len = check_tuple_len match_exp (List.length xs) in
          List.foldi xs ~init:check_len ~f:(fun idx acc x ->
            let cond = case_matched (tuple_field match_exp idx) x in
            and_ acc cond)
        | PList xs ->
          let check_len = check_list_len match_exp (List.length xs) in
          List.foldi xs ~init:check_len ~f:(fun idx acc x ->
            and_ acc (case_matched (list_field match_exp idx) x))
        | PCons (hd, tl) ->
          and_
            (not_ (list_is_empty match_exp))
            (and_
               (case_matched (list_hd match_exp) hd)
               (case_matched (list_tl match_exp) tl))
        | _ -> true_
      in
      let rec gen_match ?(has_catch_all = false) cont cases =
        let is_catch_all = function
          | PWild | PIdent _ -> true
          | _ -> false
        in
        let* match_exp = helper_expr match_exp in
        let[@warning "-8"] ((pat, action) :: tl_cases) = cases in
        let has_catch_all = has_catch_all || is_catch_all pat in
        let* action = helper_expr action in
        match tl_cases with
        | [] ->
          if has_catch_all
          then bind_pat_vars match_exp pat action |> cont
          else (
            let catch_all = panic () in
            let* then_branch = bind_pat_vars match_exp pat action in
            let cond = case_matched match_exp pat in
            cont (return (s_if_else (cond) then_branch catch_all)))
        | _ ->
          gen_match
            ~has_catch_all
            (fun else_branch ->
              let* then_branch = bind_pat_vars match_exp pat action in
              let* else_branch = else_branch in
              let cond = case_matched match_exp pat in
              match cond with
              | SConst (CBool true) -> return then_branch |> cont
              | SConst (CBool false) -> return else_branch |> cont
              | _ -> cont (return (s_if_else cond then_branch else_branch)))
            tl_cases
      in
      gen_match Fn.id cases
  and helper_def = function
    | DLet (r, p, e) ->
      let* e = helper_expr e in
      let p = pat_as_id p in
      s_let_flag r p e |> return
  in
  List.map prog ~f:(fun def -> run (helper_def def))
;;

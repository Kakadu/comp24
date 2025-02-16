(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Ast
open Tast
open Types
open Utils
open State.IntStateM
open State.IntStateM.Syntax

let true_ = TEConst (dummy_ty, CBool true)
let check_eq l r = TEApp (bool_typ, TEApp (dummy_ty, TEVar (dummy_ty, "( = )"), l), r)
let panic () = TEApp (unit_typ, TEVar (dummy_ty, "panic"), TEConst (unit_typ, CUnit))

let and_ l r =
  match l, r with
  | TEConst (_, CBool true), _ | _, TEConst (_, CBool false) -> r
  | _, TEConst (_, CBool true) | TEConst (_, CBool false), _ -> l
  | _ -> TEApp (bool_typ, TEApp (dummy_ty, TEVar (dummy_ty, "( && )"), l), r)
;;

let not_ e = TEApp (bool_typ, TEVar (dummy_ty, "not"), e)

let tuple_field lst idx =
  te_app
    dummy_ty
    (te_app dummy_ty (te_var dummy_ty "`get_tuple_field") lst)
    (te_const dummy_ty (CInt idx))
;;

let check_tuple_len tup len =
  check_eq
    (te_app int_typ (te_var dummy_ty "`tuple_len") tup)
    (te_const int_typ (CInt len))
;;

let check_list_len lst len =
  check_eq
    (te_app int_typ (te_var dummy_ty "`list_len") lst)
    (te_const int_typ (CInt len))
;;

let list_field lst idx =
  te_app
    dummy_ty
    (te_app dummy_ty (te_var dummy_ty "`list_field") lst)
    (te_const dummy_ty (CInt idx))
;;

let list_hd lst = te_app dummy_ty (te_var dummy_ty "`list_hd") lst
let list_tl lst = te_app dummy_ty (te_var dummy_ty "`list_tl") lst
let list_is_empty lst = te_app dummy_ty (te_var dummy_ty "`list_is_empty") lst

let remove_match prog =
  let rec helper_expr = function
    | TEConst _ as c -> return c
    | TEVar _ as v -> return v
    | TEApp (t, l, r) ->
      let* l = helper_expr l in
      let* r = helper_expr r in
      return (te_app t l r)
    | TEIfElse (ty, i, t, e) ->
      let* i = helper_expr i in
      let* t = helper_expr t in
      let* e = helper_expr e in
      return (te_if_else ty i t e)
    | TEFun (t, p, e) ->
      let* e = helper_expr e in
      return (te_fun t p e)
    | TELetIn (t, d, e) ->
      let* d = helper_def d in
      let* e = helper_expr e in
      return (te_let_in t d e)
    | TETuple (t, x1, x2, xs) ->
      let* x1 = helper_expr x1 in
      let* x2 = helper_expr x2 in
      let* xs =
        State.mfold_right xs ~init:[] ~f:(fun e acc ->
          let* e = helper_expr e in
          return (e :: acc))
      in
      return (te_tuple t x1 x2 xs)
    | TEList (t, xs) ->
      let* xs =
        State.mfold_right xs ~init:[] ~f:(fun e acc ->
          let* e = helper_expr e in
          return (e :: acc))
      in
      return (te_list t xs)
    | TEMatch (t, match_exp, cases) ->
      let rec bind_pat_vars match_exp pat action =
        match pat with
        | PIdent _ -> te_let_in dummy_ty (td_let dummy_ty pat match_exp) action |> return
        | PTuple (x1, x2, xs) ->
          let xs = x1 :: x2 :: xs in
          let* tuple_id = fresh_postfix >>| fun f -> "`tuple" ^ f in
          let* body =
            State.mfoldi_right xs ~init:action ~f:(fun idx acc x ->
              bind_pat_vars (tuple_field (te_var dummy_ty tuple_id) idx) x acc)
          in
          te_let_in dummy_ty (td_let dummy_ty (p_ident tuple_id) match_exp) body |> return
        | PList xs ->
          let* list_id = fresh_postfix >>| fun f -> "`list" ^ f in
          let* body =
            State.mfoldi_right xs ~init:action ~f:(fun idx action x ->
              bind_pat_vars (list_field (te_var dummy_ty list_id) idx) x action)
          in
          te_let_in dummy_ty (td_let dummy_ty (p_ident list_id) match_exp) body |> return
        | PCons (hd, tl) ->
          let* tl = bind_pat_vars (list_tl match_exp) tl action in
          bind_pat_vars (list_hd match_exp) hd tl
        | _ -> return action
      in
      let rec case_matched match_exp = function
        | PConst (CInt _ as c) | PConst (CBool _ as c) ->
          check_eq match_exp (TEConst (dummy_ty, c))
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
            cont
              (return (te_if_else t (case_matched match_exp pat) then_branch catch_all)))
        | _ ->
          gen_match
            ~has_catch_all
            (fun else_branch ->
              let* then_branch = bind_pat_vars match_exp pat action in
              let* else_branch = else_branch in
              cont
                (return
                   (te_if_else t (case_matched match_exp pat) then_branch else_branch)))
            tl_cases
      in
      gen_match Fn.id cases
  and helper_def = function
    | TDLet (t, r, p, e) ->
      let* e = helper_expr e in
      td_let_flag r t p e |> return
  in
  List.map prog ~f:(fun def -> run (helper_def def))
;;

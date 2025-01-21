open Base
open Ast
open Tast
open Types
open Utils

let tuple_field lst idx =
  te_app
    dummy_ty
    (te_app dummy_ty (te_var dummy_ty "`get_tuple_field") lst)
    (te_const dummy_ty (CInt idx))
;;

let list_field lst idx =
  te_app
    dummy_ty
    (te_app dummy_ty (te_var dummy_ty "`list_field") lst)
    (te_const dummy_ty (CInt idx))
;;

(* TODO: arrow type for CC? *)
let list_hd lst = te_app dummy_ty (te_var dummy_ty "`list_hd") lst
let list_tl lst = te_app dummy_ty (te_var dummy_ty "`list_tl") lst
let list_is_empty lst = te_app dummy_ty (te_var dummy_ty "`list_is_empty") lst
let check_eq l r = TEApp (dummy_ty, TEApp (dummy_ty, TEVar (dummy_ty, "( = )"), l), r)
let and_ l r = TEApp (dummy_ty, TEApp (dummy_ty, TEVar (dummy_ty, "( && )"), l), r)

let remove_match =
  let rec helper_expr = function
    | TEConst _ as c -> c
    | TEVar _ as v -> v
    | TEApp (t, l, r) -> te_app t (helper_expr l) (helper_expr r)
    | TEIfElse (ty, i, t, e) ->
      te_if_else ty (helper_expr i) (helper_expr t) (helper_expr e)
    | TEFun (t, p, e) -> te_fun t p (helper_expr e)
    | TELetIn (t, d, e) -> te_let_in t (helper_def d) (helper_expr e)
    | TETuple (t, xs) -> te_tuple t (List.map xs ~f:helper_expr)
    | TEList (t, xs) -> te_list t (List.map xs ~f:helper_expr)
    | TEMatch (t, match_exp, cases) ->
      let rec bind_pat_vars match_exp pat action =
        match pat with
        | PIdent _ -> te_let_in dummy_ty (td_let dummy_ty pat match_exp) action
        | PTuple xs ->
          (* TODO: simplify `&& true` *)
          te_let_in
            dummy_ty
            (td_let dummy_ty (p_ident "`tuple") match_exp)
            (List.foldi xs ~init:action ~f:(fun idx action x ->
               bind_pat_vars (tuple_field (te_var dummy_ty "`tuple") idx) x action))
        | PList xs ->
          te_let_in
            dummy_ty
            (td_let dummy_ty (p_ident "`list") match_exp)
            (List.foldi xs ~init:action ~f:(fun idx action x ->
               bind_pat_vars (list_field (te_var dummy_ty "`list") idx) x action))
        | PCons (hd, tl) ->
          bind_pat_vars
            (list_hd match_exp)
            hd
            (bind_pat_vars (list_tl match_exp) tl action)
        | _ -> action
      in
      let rec case_matched match_exp = function
        | PConst (CInt _ as c) | PConst (CBool _ as c) ->
          check_eq match_exp (TEConst (dummy_ty, c))
        | PConst CNil -> list_is_empty match_exp
        | PTuple xs ->
          List.foldi
            xs
            ~init:(TEConst (dummy_ty, CBool true))
            ~f:(fun idx acc x -> and_ acc (case_matched (tuple_field match_exp idx) x))
        | PList xs -> 
          List.foldi
            xs
            ~init:(TEConst (dummy_ty, CBool true))
            ~f:(fun idx acc x -> and_ acc (case_matched (list_field match_exp idx) x))
        | PCons (hd, tl) ->
          and_ (case_matched (list_hd match_exp) hd) (case_matched (list_tl match_exp) tl)
        | _ -> TEConst (dummy_ty, CBool true)
      in
      let rec gen_match cont cases =
        let match_exp = helper_expr match_exp in
        let[@warning "-8"] ((pat, action) :: tl_cases) = cases in
        let action = helper_expr action in
        match tl_cases with
        | [] -> bind_pat_vars match_exp pat action |> cont
        | _ ->
          gen_match
            (fun else_branch ->
              cont
                (te_if_else
                   t
                   (case_matched match_exp pat)
                   (bind_pat_vars match_exp pat action)
                   else_branch))
            tl_cases
      in
      gen_match Fn.id cases
  and helper_def = function
    | TDLet (t, r, p, e) ->
      td_let_flag r t p (helper_expr e)
  in
  List.map ~f:helper_def
;;

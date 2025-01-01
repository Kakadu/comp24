open Base
open Ast
open Tast
open Types

let tuple_field lst idx =
  te_app
    dummy_ty
    (te_app dummy_ty (te_var dummy_ty "`tuple_field") lst)
    (te_const dummy_ty (CInt idx))
;;

let list_field lst idx =
  te_app
    dummy_ty
    (te_app dummy_ty (te_var dummy_ty "`list_field") lst)
    (te_const dummy_ty (CInt idx))
;;

let check_eq l r = TEApp (dummy_ty, TEApp (dummy_ty, TEVar (dummy_ty, "( = )"), l), r)

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
          List.foldi xs ~init:action ~f:(fun idx action x ->
            bind_pat_vars (tuple_field match_exp idx) x action)
        | PList xs ->
          List.foldi xs ~init:action ~f:(fun idx action x ->
            bind_pat_vars (list_field match_exp idx) x action)
        | PCons (hd, tl) -> failwith "TODO bind_pat_vars cons"
        | _ -> action
      in
      let case_matched match_exp = function
        | PConst c -> check_eq match_exp (TEConst (dummy_ty, c))
        | PTuple xs -> failwith "TODO case_matched tuple"
        | PList xs -> failwith "TODO case_matched list"
        | PCons (hd, tl) -> failwith "TODO case_matched cons"
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
                   (texpr_type action)
                   (case_matched match_exp pat)
                   (bind_pat_vars match_exp pat action)
                   else_branch))
            tl_cases
      in
      gen_match Fn.id cases
  and helper_def = function
    | TDLet (t, r, p, e) ->
      (* TODO: remove_patterns.ml ? *)
      td_let_flag r t p (helper_expr e)
  in
  List.map ~f:helper_def
;;

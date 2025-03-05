(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Monads.CounterMonad (String)
open Anf_ast
open Ll_ast
open Match_elimination.Me_ast

type bind = ABind of me_ident * cexpr

let new_me_name =
  let open Common.Naming in
  let+ num = fresh in
  Me_name (with_pref anf_prefix @@ Int.to_string num)
;;

let const_to_immexpr =
  let open Common.Ast in
  function
  | Const_int v -> Imm_int v
  | Const_bool v -> Imm_bool v
  | Const_nil -> Imm_nil
  | Const_unit -> Imm_unit
;;

let id_to_immexpr v = Imm_id v
let immexpr_to_cexpr v = C_immexpr v

let optimize_cons_to_rlist : ll_expr -> ll_expr -> ll_expr list =
  fun head_pt tail_pt ->
  let rec helper acc = function
    | LL_list (e1, e2) -> helper (e1 :: acc) e2
    | end_pt -> end_pt :: acc
  in
  helper [ head_pt ] tail_pt
;;

let check_rlist_invariant = function
  | (LL_const Const_nil as lst) :: pref -> return (lst, pref)
  | _ -> fail "First element in reversed list should be a nil"
;;

let rec to_immexpr : ll_expr -> (bind list * immexpr) t = function
  | LL_const v -> return ([], const_to_immexpr v)
  | LL_ident id -> return ([], id_to_immexpr id)
  | expr ->
    let* id = new_me_name in
    let+ binds, cexpr = to_cexpr expr in
    let new_bind = ABind (id, cexpr) in
    binds @ [ new_bind ], id_to_immexpr id

and to_cexpr : ll_expr -> (bind list * cexpr) t =
  let open Base in
  function
  | LL_const v -> return ([], C_immexpr (const_to_immexpr v))
  | LL_ident id -> return ([], C_immexpr (id_to_immexpr id))
  | LL_let (id, expr, in') ->
    let* e_binds, cexpr = to_cexpr expr in
    let+ in_binds, in'' = to_cexpr in' in
    let let_bind = ABind (id, cexpr) in
    e_binds @ [ let_bind ] @ in_binds, in''
  | LL_ifthenelse (i, t, e) ->
    let* i_binds, i_cexpr = to_immexpr i in
    let* t_aexpr = to_aexpr t in
    let+ e_aexpr = to_aexpr e in
    i_binds, C_ifthenelse (i_cexpr, t_aexpr, e_aexpr)
  | LL_tuple tup'l ->
    let+ binds_n_vals = mapt tup'l to_immexpr in
    let binds'l, values'l = List.unzip binds_n_vals in
    let tup_binds = List.concat binds'l in
    tup_binds, C_tuple values'l
  | LL_list (h, tl) ->
    let rexpr'l = optimize_cons_to_rlist h tl in
    let* lst, pref'l = check_rlist_invariant rexpr'l in
    let* lst_bind'l, lst_immexpr = to_immexpr lst in
    let+ pref_binds_n_vals = mapt pref'l to_immexpr in
    let binds'l, values'l = List.unzip pref_binds_n_vals in
    let list_binds =
      let all_binds = lst_bind'l :: binds'l in
      List.concat all_binds
    in
    list_binds, C_rlist (lst_immexpr, values'l)
  | LL_apply (e_fun, e_arg) ->
    let optimize_apply f arg =
      (* TODO: chech args passing order correctness *)
      let rec helper fst_arg args = function
        | LL_apply (f', new_fst) -> helper new_fst (fst_arg :: args) f'
        | name_e -> name_e, fst_arg, args
      in
      helper arg [] f
    in
    let extract_name_from_imm = function
      | Imm_id (Me_name id) -> return id
      | _ ->
        fail "Apply left part expects 0 argument(s), but is applied here to 1 argument(s)"
    in
    let name_e, e_fst_arg, e_arg'l = optimize_apply e_fun e_arg in
    let* name_bind'l, imm_name = to_immexpr name_e in
    let* name = extract_name_from_imm imm_name in
    let* fst_bind'l, imm_fst = to_immexpr e_fst_arg in
    let+ binds_n_args = mapt e_arg'l to_immexpr in
    let arg_binds'l, arg'l = List.unzip binds_n_args in
    let arg_bind'l = List.concat arg_binds'l in
    let new_binds = name_bind'l @ fst_bind'l @ arg_bind'l in
    new_binds, C_apply (name, imm_fst, arg'l)

and to_aexpr : ll_expr -> aexpr t =
  let alet id cexpr aexpr = A_let (id, cexpr, aexpr) in
  fun expr ->
    let+ binds, cexpr = to_cexpr expr in
    let basic_aexpr = A_cexpr cexpr in
    let particial_aexpr =
      List.fold_left
        (fun acc (ABind (id, cvalue)) next -> acc @@ alet id cvalue next)
        (fun x -> x)
        binds
    in
    particial_aexpr basic_aexpr
;;

let to_func { lldec_name; lldec_args; lldec_body } =
  let+ adec_body = to_aexpr lldec_body in
  { adec_name = lldec_name; adec_args = lldec_args; adec_body }
;;

let to_anf_decl = function
  | LL_GlobalV (me_ident, expr) ->
    let+ aexpr = to_aexpr expr in
    A_GlobalV (me_ident, aexpr)
  | LL_Decl (r_flag, ll_fun, ll_fun'l) ->
    let* anf_fun = to_func ll_fun in
    let+ anf_fun'l = mapt ll_fun'l to_func in
    ADMutualRecDecl (r_flag, anf_fun, anf_fun'l)
;;

let to_anf_prog ll_prog = mapt ll_prog to_anf_decl

let convert_to_anf prog = 
  let open Common.Errors in
  match run (to_anf_prog prog) with
  | _, Ok anf_decl'l -> Result.Ok anf_decl'l
  | _, Error msg -> Result.Error (illegal_state msg)

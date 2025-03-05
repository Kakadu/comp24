(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Match_elimination.Me_ast
open Ll_ast

open
  Common.Monads.GenericCounterMonad
    (struct
      type t = ll_structure_item list
    end)
    (String)

let remove_saved_rdecls : ll_structure_item list t =
  let* rfuncs = read in
  let+ () = save [] in
  rfuncs
;;

let save_decl func =
  let* funcs = read in
  save @@ (func :: funcs)
;;

let save_toplevel_decl r_flag func other_funcs =
  let decl = LL_Decl (r_flag, func, other_funcs) in
  save_decl decl
;;

let new_me_name =
  let open Common.Naming in
  let+ num = fresh in
  with_pref ll_prefix @@ Int.to_string num
;;

let optimize_func arg body =
  let rec helper rargs = function
    | MExp_function (arg', body') -> helper (arg' :: rargs) body'
    | body' -> List.rev rargs, body'
  in
  let args, body' = helper [] body in
  (arg, args), body'
;;

let deconstruct_vbl = function
  | [] -> fail "Impossible case  (3)"
  | vb :: tl -> return (vb, tl)
;;

let rec to_ll_expr = function
  | MExp_constant c -> return @@ LL_const c
  | MExp_ident id -> return @@ LL_ident id
  | MExp_tuple tup ->
    let+ tup' = mapt tup to_ll_expr in
    LL_tuple tup'
  | MExp_apply (e1, e2) ->
    let* e1' = to_ll_expr e1 in
    let+ e2' = to_ll_expr e2 in
    LL_apply (e1', e2')
  | MExp_list (e1, e2) ->
    let* e1' = to_ll_expr e1 in
    let+ e2' = to_ll_expr e2 in
    LL_list (e1', e2')
  | MExp_ifthenelse (i, t, e) ->
    let* i' = to_ll_expr i in
    let* t' = to_ll_expr t in
    let+ e' = to_ll_expr e in
    LL_ifthenelse (i', t', e')
  | MExp_function (arg, body) ->
    let* func_name = new_me_name in
    let* func = lift_lambda func_name (arg, body) in
    let+ () =
      let decl = LL_Decl (Common.Ast.Nonrecursive, func, []) in
      save_decl decl
    in
    LL_ident func_name
  | MExp_let (MDecl (r_flag, vb'l), cont) ->
    (match vb'l with
     | vb :: [] ->
       (match vb.m_vb_pat, vb.m_vb_expr with
        | Id_name func_name, MExp_function (arg, body) ->
          let* func = lift_lambda func_name (arg, body) in
          let* () = save_toplevel_decl r_flag func [] in
          to_ll_expr cont
        | Id_unit, MExp_function _ -> fail "Impossible case (1)"
        | _, not_function ->
          let* ll_bind = to_ll_expr not_function in
          let+ ll_cont = to_ll_expr cont in
          LL_let (vb.m_vb_pat, ll_bind, ll_cont))
     | vb :: vbl ->
       let* func = lift_vb vb in
       let* other_funcs = mapt vbl lift_vb in
       let* () = save_toplevel_decl r_flag func other_funcs in
       to_ll_expr cont
     | [] -> fail "Impossible case (2)")

and lift_lambda name (arg, row_body) =
  let+ lldec_args, lldec_body =
    let args, body = optimize_func arg row_body in
    let+ ll_body = to_ll_expr body in
    args, ll_body
  in
  { lldec_name = name; lldec_args; lldec_body }

and lift_vb vb =
  match vb.m_vb_pat, vb.m_vb_expr with
  | Id_name func_name, MExp_function (arg, body) -> lift_lambda func_name (arg, body)
  | _ -> fail "And can be used only with mutualrec cases [func and func and ... and func]"
;;

let to_ll_structure_item_reversed (MDecl (r_flag, vb'l)) =
  let* ll_str =
    match vb'l with
    | vb :: [] ->
      (match vb.m_vb_pat, vb.m_vb_expr with
       | Id_name func_name, MExp_function (arg, body) ->
         let+ func = lift_lambda func_name (arg, body) in
         LL_Decl (r_flag, func, [])
       | Id_unit, MExp_function _ -> fail "Impossible case (1)"
       | _, not_function ->
         let+ ll_bind = to_ll_expr not_function in
         LL_GlobalV (vb.m_vb_pat, ll_bind))
    | vb :: vbl ->
      let* func = lift_vb vb in
      let+ other_funcs = mapt vbl lift_vb in
      LL_Decl (r_flag, func, other_funcs)
    | [] -> fail "Impossible case (2)"
  in
  let+ lifted_rdecls = remove_saved_rdecls in
  List.rev @@ (ll_str :: lifted_rdecls)
;;

let to_ll_program prog =
  let+ reversed_ll_str'l'l = mapt prog to_ll_structure_item_reversed in
  List.concat @@ reversed_ll_str'l'l
;;

let lift_lambdas prog =
  let open Common.Errors in
  match run (to_ll_program prog) [] with
  | _, Ok ll_prog -> Ok ll_prog
  | _, Error msg -> Result.Error (illegal_state msg)
;;

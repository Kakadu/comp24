(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module CounterMonad = struct
  type fresh_id = int

  let name_prefix = Common.Naming.me_prefix
  let fail_f = Me_ast.MExp_ident "rt_fail_pt_match" (*TODO: mb make apply*)
  let get_tuple_elem_f = Me_ast.MExp_ident "rt_fail_pt_match"

  include Common.Se_monad.Base_SE_Monad

  type 'a t = (fresh_id, 'a, string) Common.Se_monad.Base_SE_Monad.t

  let get_uniq_name : string t =
    let* old_id = read in
    let new_id = old_id + 1 in
    let* () = save new_id in
    return @@ name_prefix ^ "_" ^ Int.to_string new_id
  ;;
end

open CounterMonad
open Common.Ast
open Me_ast

let to_me_const c = return c

(** bind * condition * then_branch *)
type alt_pat = ident * m_expr * (m_expr -> m_expr)

(* let optimize_cons = [(::) pt1, [(::) pt2, pt3)]] --> pt1 ; pt2 ; pt3 *)

(* let create_alt_pat *)
let fail_app arg = MExp_apply (fail_f, arg)
let me_name nm = MExp_ident nm

let complete_with (_, cond, then_f) then_e else_br =
  let then_br = then_f then_e in
  MExp_ifthenelse (cond, then_br, else_br)
;;

let complete_with_fail (nm, cond, then_f) =
  let else_br = fail_app @@ me_name nm in
  complete_with (nm, cond, then_f) else_br
;;

let me_nonrec_letin nm_s body cont =
  let bind = [ { m_vb_pat = Me_name nm_s; m_vb_expr = body } ] in
  MExp_let (MDecl (Nonrecursive, bind), cont)
;;

let me_2n_op o arg1 arg2 = MExp_apply (MExp_apply (o, arg1), arg2)

let me_const_comperison c1 me =
  let comp_name = MExp_ident Common.Base_lib.op_eq in
  let const = MExp_constant c1 in
  me_2n_op comp_name const me
;;

let me_and_func = me_2n_op (me_name Common.Base_lib.op_and)
let me_bool v = MExp_constant (Const_bool v)

let get_tup_elem e i =
  me_2n_op get_tuple_elem_f e (MExp_constant (Common.Ast.Const_int i))
;;

let rec elim_pattern : pattern -> alt_pat t =
  fun pt ->
  let merge_alt_pts (acc_nm, acc_cond, acc_then_f) (nm, cond, then_f) =
    let new_cond = me_and_func cond acc_cond in
    let new_then_f then_e =
      let new_bind_v = me_name nm in
      let acc_let_binding = me_nonrec_letin acc_nm new_bind_v in
      then_f @@ acc_let_binding @@ acc_then_f then_e
    in
    return @@ (nm, new_cond, new_then_f)
  in
  match pt with
  | Pat_var bind_nm -> return @@ (bind_nm, me_bool true, fun then_e -> then_e)
  | Pat_any ->
    let* bind_nm = get_uniq_name in
    return @@ (bind_nm, me_bool true, fun then_e -> then_e)
  | Pat_const c ->
    let* bind_name = get_uniq_name in
    let me_cond = me_const_comperison c (MExp_ident bind_name) in
    return @@ (bind_name, me_cond, fun then_e -> then_e)
  | Pat_cons (pt1, pt2) ->
    let* alt_pt1 = elim_pattern pt1 in
    let* alt_pt2 = elim_pattern pt2 in
    merge_alt_pts alt_pt1 alt_pt2
  | Pat_tuple pt'list ->
    let* alt_pts =
      fold_left_t pt'list ~init:(return []) ~f:(fun acc pt ->
        let* pt' = elim_pattern pt in
        return @@ (pt' :: acc))
    in
    let* alt_pt, alt_pts =
      match alt_pts with
      | alt_pt :: alt_pts -> return @@ (alt_pt, alt_pts)
      | _ -> fail "Tuple invariant: n >= 2 should be"
    in
    fold_left_t alt_pts ~init:(return alt_pt) ~f:merge_alt_pts
  | Pat_constraint (pt, _) -> elim_pattern pt
;;

let rec to_me_expr e =
  match e with
  | Exp_constant c -> return @@ MExp_constant c
  | Exp_ident name -> return @@ MExp_ident name
  | Exp_type (e', _) -> to_me_expr e'
  | Exp_tuple e'list ->
    let* me'list =
      revt
      @@ fold_left_t e'list ~init:(return []) ~f:(fun acc e' ->
        let* me = to_me_expr e' in
        return @@ (me :: acc))
    in
    return @@ MExp_tuple me'list
  | Exp_apply (e'fst, e'snd) ->
    let* me'fst = to_me_expr e'fst in
    let* me'snd = to_me_expr e'snd in
    return @@ MExp_apply (me'fst, me'snd)
  | Exp_list (e'fst, e'snd) ->
    let* me'fst = to_me_expr e'fst in
    let* me'snd = to_me_expr e'snd in
    return @@ MExp_list (me'fst, me'snd)
  | Exp_ifthenelse (e1, e2, e3) ->
    let* e1' = to_me_expr e1 in
    let* e2' = to_me_expr e2 in
    let* e3' = to_me_expr e3 in
    return @@ MExp_ifthenelse (e1', e2', e3')
  | Exp_function (pt, e') ->
    let* fun_body = to_me_expr e' in
    let* nm, cond, then_f = elim_pattern pt in
    let fun_body' = complete_with_fail (nm, cond, then_f) fun_body in
    return @@ MExp_function (Me_name nm, fun_body')
    (* TODO: *)
  | Exp_let (Decl (r_flag, vb_l), e) ->
    let* vb_l' =
      revt
      @@ fold_left_t vb_l ~init:(return []) ~f:(fun acc vb ->
        match vb.vb_pat with
        | Pat_const _ | Pat_tuple _ | Pat_cons _ -> fail "TODO: deconstructor"
        | pt ->
          let* bind_body = to_me_expr vb.vb_expr in
          let* nm, cond, then_f = elim_pattern pt in
          let bind_body' = complete_with_fail (nm, cond, then_f) bind_body in
          return @@ ({ m_vb_pat = Me_name nm; m_vb_expr = bind_body' } :: acc))
    in
    let* e' = to_me_expr e in
    return @@ MExp_let (MDecl (r_flag, vb_l'), e')
  | Exp_match (main_e, branches) ->
    let* e_for_match' = to_me_expr main_e in
    let* bind_name = get_uniq_name in
    let* f_match' =
      fold_left_t
        branches
        ~init:(return (me_nonrec_letin bind_name e_for_match'))
        ~f:(fun acc_f (pt, br_e) ->
          let* br_e' = to_me_expr br_e in
          let* nm, cond, then_f = elim_pattern pt in
          let new_acc_f cont =
            acc_f
            @@ me_nonrec_letin nm (MExp_ident bind_name)
            @@ complete_with (nm, cond, then_f) br_e' cont
          in
          return new_acc_f)
    in
    return @@ f_match' fail_f
;;

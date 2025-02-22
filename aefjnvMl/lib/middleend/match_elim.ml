(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module CounterMonad = struct
  type fresh_id = int

  let name_prefix = Common.Naming.me_prefix
  let fail_f = Me_ast.MExp_ident "rt_fail_pt_match" (*TODO: mb make apply*)
  let get_by_idx_f = Me_ast.MExp_ident "rt_get_by_idx"
  let get_list_len_1_f = Me_ast.MExp_ident "rt_get_list_len_plus_one"

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

(** bind * condition * then_branch *)
type alt_pat = ident * m_expr * (m_expr -> m_expr)

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
let me_eq = me_2n_op @@ MExp_ident Common.Base_lib.op_eq
let me_more_eq = me_2n_op @@ MExp_ident Common.Base_lib.op_more_eq

let me_const_comperison c1 me =
  let const = MExp_constant c1 in
  me_eq const me
;;

let me_and_func = me_2n_op (me_name Common.Base_lib.op_and)
let me_bool v = MExp_constant (Const_bool v)
let me_get_by_idx e i = me_2n_op get_by_idx_f e (MExp_constant (Common.Ast.Const_int i))
let me_get_list_len_1 me = MExp_apply (get_list_len_1_f, me)

(** [(::) pt1, ((::) pt2, pt3))] --> [pt1 ; pt2 ; pt3] *)
let optimize_cons : pattern -> pattern -> pattern list =
  fun head_pt tail_pt ->
  List.rev
  @@
  let rec helper acc = function
    | Pat_cons (pt1, pt2) -> helper (pt1 :: acc) pt2
    | end_pt -> end_pt :: acc
  in
  helper [ head_pt ] tail_pt
;;

let rec elim_pattern : pattern -> alt_pat t =
  let elim_pt_list pt'list =
    let elim_pt_list_rev pt_list =
      let* indexed_alt_pts, _ =
        fold_left_t
          pt_list
          ~init:(return ([], 0))
          ~f:(fun (acc, idx) pt ->
            let* pt' = elim_pattern pt in
            return @@ ((pt', idx) :: acc, idx + 1))
      in
      return indexed_alt_pts
    in
    let merge_alt_pts'list indexed_alt_pts =
      let* tup_bind_name = get_uniq_name in
      let* bindings =
        let me_tup_bind_name = me_name tup_bind_name in
        fold_left_t
          indexed_alt_pts
          ~init:(return (fun cont -> cont))
          ~f:(fun acc_f (alt_pt, idx) ->
            let under_bind_nm, _, _ = alt_pt in
            let get_elem_from_bind = me_get_by_idx me_tup_bind_name idx in
            let new_acc_f cont =
              me_nonrec_letin under_bind_nm get_elem_from_bind @@ acc_f cont
            in
            return @@ new_acc_f)
      in
      let* (alt_pt, _), alt_pts_tl =
        match indexed_alt_pts with
        | alt_pt :: alt_pts -> return @@ (alt_pt, alt_pts)
        | _ -> fail "Tuple invariant: n >= 2 should be"
      in
      let* common_cond =
        let* unbounded_common_cond =
          let _, cond_e, _ = alt_pt in
          fold_left_t alt_pts_tl ~init:(return @@ cond_e) ~f:(fun acc_e (alt_pt, _) ->
            let _, cur_cond, _ = alt_pt in
            let merged_cond = me_and_func cur_cond acc_e in
            return merged_cond)
        in
        return @@ bindings unbounded_common_cond
      in
      let* common_body =
        let* unbounded_common_body =
          let _, _, then_f = alt_pt in
          fold_left_t alt_pts_tl ~init:(return @@ then_f) ~f:(fun acc_f (alt_pt, _) ->
            let _, _, cur_then_f = alt_pt in
            let merged_then_f cont = cur_then_f @@ acc_f cont in
            return @@ merged_then_f)
        in
        return @@ fun cont -> bindings @@ unbounded_common_body cont
      in
      return (tup_bind_name, common_cond, common_body)
    in
    let* indexed_alt_pts = elim_pt_list_rev pt'list in
    merge_alt_pts'list indexed_alt_pts
  in
  fun pt ->
    match pt with
    | Pat_var bind_nm -> return @@ (bind_nm, me_bool true, fun then_e -> then_e)
    | Pat_any ->
      let* bind_nm = get_uniq_name in
      return @@ (bind_nm, me_bool true, fun then_e -> then_e)
    | Pat_const c ->
      let* bind_name = get_uniq_name in
      let me_cond = me_const_comperison c (MExp_ident bind_name) in
      return @@ (bind_name, me_cond, fun then_e -> then_e)
    | Pat_constraint (pt, _) -> elim_pattern pt
    | Pat_tuple pt'list -> elim_pt_list pt'list
    | Pat_cons (head_pt, tail_pt) ->
      let plain_list = optimize_cons head_pt tail_pt in
      let me_pt_elem_cnt = MExp_constant (Const_int (List.length plain_list)) in
      let* bind_nm, cond, then_br = elim_pt_list plain_list in
      let* last_elem =
        match List.rev plain_list with
        | x :: [] -> return x
        | _ -> fail "Pat_cons has at least two elements"
      in
      let len_cond_me =
        let me_list_len = me_get_list_len_1 @@ me_name bind_nm in
        match last_elem with
        | Pat_const Const_nil -> me_eq me_pt_elem_cnt me_list_len
        | _ -> me_more_eq me_list_len me_pt_elem_cnt
      in
      let lazy_if_cond = MExp_ifthenelse (len_cond_me, cond, me_bool false) in
      return @@ (bind_nm, lazy_if_cond, then_br)
;;

let to_me_vbs to_me_expr { vb_pat; vb_expr } =
  match vb_pat with
  | Pat_const _ | Pat_tuple _ | Pat_cons _ ->
    fail "TODO: rewrite deconstructors as mne matches and use [to_me_expr]"
  | pt ->
    let* bind_body = to_me_expr vb_expr in
    let* nm, cond, then_f = elim_pattern pt in
    let bind_body' = complete_with_fail (nm, cond, then_f) bind_body in
    return [ { m_vb_pat = Me_name nm; m_vb_expr = bind_body' } ]
;;

let elim_pats_in_vbl to_me_expr vb_l =
  revt
  @@ fold_left_t vb_l ~init:(return []) ~f:(fun acc vb ->
    let* vb'_l = to_me_vbs to_me_expr vb in
    return @@ List.concat [ vb'_l; acc ])
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
  | Exp_let (Decl (r_flag, vb_l), e) ->
    let* vb_l' = elim_pats_in_vbl to_me_expr vb_l in
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

let to_me_struct_item = function
  | Str_eval e ->
    let* new_uniq_bind_nm = get_uniq_name in
    let m_vb_pat = Me_name new_uniq_bind_nm in
    let* m_vb_expr = to_me_expr e in
    return @@ MDecl (Nonrecursive, [ { m_vb_pat; m_vb_expr } ])
  | Str_value (Decl (rflag, vb_l)) ->
    let* vb_l' = elim_pats_in_vbl to_me_expr vb_l in
    return @@ MDecl (rflag, vb_l')
;;

let to_me_program prog =
  revt
  @@ fold_left_t prog ~init:(return []) ~f:(fun acc str_item ->
    let* m_decl = to_me_struct_item str_item in
    return @@ (m_decl :: acc))
;;

let eliminate_match_in_program prog =
  let open Common.Errors in
  match run (to_me_program prog) 0 with
  | _, Ok me_decl'list -> Result.Ok me_decl'list
  | _, Error msg -> Result.Error (illegal_state msg)
;;

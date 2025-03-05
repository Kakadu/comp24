(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Me_ast
open Common.Monads.CounterMonad (String)

let get_uniq_name : string t =
  let open Common.Naming in
  let+ new_id = fresh in
  with_pref me_prefix @@ Int.to_string new_id
;;

type semantic_bind = SBind of string id_t * m_expr

type alt_pt_tp =
  | Any of semantic_bind
  | Var of semantic_bind
  | Const of m_expr (** condition *)
  | Compound of m_expr option * semantic_bind list

type alt_pat = alt_pt_tp

let fail_f = Me_ast.MExp_ident Common.Base_lib.func_fail_pt_match
let get_by_idx_f = Me_ast.MExp_ident Common.Base_lib.func_get_by_idx
let get_list_len_1_f = Me_ast.MExp_ident Common.Base_lib.func_get_list_len_plus_one
let fail_app = MExp_apply (fail_f, MExp_constant Const_unit)
let me_name nm = MExp_ident nm
let ite ~cond ~then_br ~else_br = MExp_ifthenelse (cond, then_br, else_br)

let ite_with_fail ~cond ~then_br =
  let else_br = fail_app in
  ite ~cond ~then_br ~else_br
;;

let me_nonrec_letin nm body cont =
  let bind = [ { m_vb_pat = nm; m_vb_expr = body } ] in
  MExp_let (MDecl (Nonrecursive, bind), cont)
;;

let me_2n_op o arg1 arg2 = MExp_apply (MExp_apply (o, arg1), arg2)
let me_eq = me_2n_op @@ MExp_ident Common.Base_lib.op_eq
let me_more_eq = me_2n_op @@ MExp_ident Common.Base_lib.op_more_eq

let me_const_comperison c1 me =
  let const = MExp_constant c1 in
  me_eq const me
;;

let me_vb m_vb_pat m_vb_expr = { m_vb_pat; m_vb_expr }
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

let polimorfic_to_letin f values =
  List.fold_left
    (fun acc_f v cont ->
      let nm, me = f v in
      acc_f @@ me_nonrec_letin nm me cont)
    (fun cont -> cont)
    values
;;

let sbinds_to_nonrec_letin sbinds =
  polimorfic_to_letin (fun (SBind (nm, me)) -> nm, me) sbinds
;;

let vbs_to_nonrec_letin vbs =
  polimorfic_to_letin (fun { m_vb_pat; m_vb_expr } -> m_vb_pat, m_vb_expr) vbs
;;

let sbind_to_me_vb (SBind (id, me)) = me_vb id me

let rec elim_pattern : m_expr -> pattern -> alt_pat t =
  fun mexpr ->
  let update_cond c_cond = function
    | Some cond -> me_and_func c_cond cond
    | None -> c_cond
  in
  let row_compound_alt_pat pt'list =
    (* REVERSE *)
    let* _, row_binds =
      fold_left_t
        pt'list
        ~init:(return (0, []))
        ~f:(fun (idx, acc) pt ->
          let tuple_elem = me_get_by_idx mexpr idx in
          let+ alt_pt = elim_pattern tuple_elem pt in
          let new_acc = alt_pt :: acc in
          idx + 1, new_acc)
    in
    (* REVERSE-BACK *)
    let cond_opt, provided_sbinds =
      List.fold_left
        (fun acc alt_pat ->
          let old_cond_opt, provided_sbinds = acc in
          match alt_pat with
          | Any _ -> old_cond_opt, provided_sbinds
          | Var var_bind -> old_cond_opt, var_bind :: provided_sbinds
          | Const c_cond ->
            let updated_cond = update_cond c_cond old_cond_opt in
            Some updated_cond, provided_sbinds
          | Compound (c_cond_opt, c_sbinds) ->
            let updated_sbinds = c_sbinds @ provided_sbinds in
            let updated_cond =
              match c_cond_opt with
              | Some c_cond ->
                let new_cond = update_cond c_cond old_cond_opt in
                Some new_cond
              | None -> old_cond_opt
            in
            updated_cond, updated_sbinds)
        (None, [])
        row_binds
    in
    return (cond_opt, provided_sbinds)
  in
  function
  | Pat_var nm -> return @@ Var (SBind (Id_name nm, mexpr))
  | Pat_any ->
    let* any_name = get_uniq_name in
    return @@ Any (SBind (Id_name any_name, mexpr))
  | Pat_const Const_unit -> return @@ Var (SBind (Id_unit, mexpr))
  | Pat_const c ->
    let me_cond = me_const_comperison c mexpr in
    return @@ Const me_cond
  | Pat_constraint (pt, _) -> elim_pattern mexpr pt
  | Pat_tuple pt'list ->
    let+ compound_cond, provided_sbinds = row_compound_alt_pat pt'list in
    Compound (compound_cond, provided_sbinds)
  | Pat_cons (head_pt, tail_pt) ->
    let plain_pat_list = optimize_cons head_pt tail_pt in
    let* last_pat =
      match List.rev plain_pat_list with
      | x :: _ -> return x
      | _ -> fail "Pat_cons has at least two elements"
    in
    let+ compound_cond, provided_sbinds = row_compound_alt_pat plain_pat_list in
    let len_cond_me =
      let me_pt_elem_cnt = MExp_constant (Const_int (List.length plain_pat_list)) in
      let me_list_len = me_get_list_len_1 @@ mexpr in
      match last_pat with
      | Pat_const Const_nil -> me_eq me_pt_elem_cnt me_list_len
      | _ -> me_more_eq me_list_len me_pt_elem_cnt
    in
    let particial_alt_pt cond = Compound (Some cond, provided_sbinds) in
    (match compound_cond with
     | None ->
       let alt_pat = particial_alt_pt len_cond_me in
       alt_pat
     | Some cond ->
       let new_cond = MExp_ifthenelse (len_cond_me, cond, me_bool false) in
       let alt_pat = particial_alt_pt new_cond in
       alt_pat)
;;

let to_me_vb to_me_expr { vb_pat; vb_expr } =
  let generate_check_vb cond =
    let placeholder = MExp_constant Const_unit in
    let check_body = ite_with_fail ~cond ~then_br:placeholder in
    let+ check_nm = get_uniq_name in
    let check_nm_me = Id_name (check_nm ^ "_ANY") in
    me_vb check_nm_me check_body
  in
  let* bind_name = get_uniq_name in
  let main_nm = me_name bind_name in
  let* alt_pat = elim_pattern main_nm vb_pat in
  let* bind_body = to_me_expr vb_expr in
  match alt_pat with
  | Var (SBind (name, _)) | Any (SBind (name, _)) -> return @@ [ me_vb name bind_body ]
  | Const cond ->
    let main_vb = me_vb (Id_name bind_name) bind_body in
    let+ check_vb = generate_check_vb cond in
    [ main_vb; check_vb ]
  | Compound (cond_opt, sbinds) ->
    let provided_vbs = List.map sbind_to_me_vb sbinds in
    let main_vb = me_vb (Id_name bind_name) bind_body in
    (match cond_opt with
     | Some cond ->
       let+ check_vb = generate_check_vb cond in
       [ main_vb; check_vb ] @ provided_vbs
     | None -> return @@ (main_vb :: provided_vbs))
;;

(* TODO: remove redudant and (refactor with letin or (;;) ) *)
let elim_vb'list to_me_expr vb_l =
  let+ list_of_list = mapt vb_l (to_me_vb to_me_expr) in
  List.concat list_of_list
;;

let rec to_me_expr = function
  | Exp_constant c -> return @@ MExp_constant c
  | Exp_ident name -> return @@ MExp_ident name
  | Exp_type (e', _) -> to_me_expr e'
  | Exp_tuple e'list ->
    let+ me'list = mapt e'list to_me_expr in
    MExp_tuple me'list
  | Exp_apply (e'fst, e'snd) ->
    let* me'fst = to_me_expr e'fst in
    let+ me'snd = to_me_expr e'snd in
    MExp_apply (me'fst, me'snd)
  | Exp_list (e'fst, e'snd) ->
    let* me'fst = to_me_expr e'fst in
    let+ me'snd = to_me_expr e'snd in
    MExp_list (me'fst, me'snd)
  | Exp_ifthenelse (e1, e2, e3) ->
    let* e1' = to_me_expr e1 in
    let* e2' = to_me_expr e2 in
    let+ e3' = to_me_expr e3 in
    MExp_ifthenelse (e1', e2', e3')
  | Exp_let (Decl (r_flag, vb_l), e) ->
    let* e' = to_me_expr e in
    let+ vb_l' = elim_vb'list to_me_expr vb_l in
    (match r_flag with
     | Recursive -> MExp_let (MDecl (Recursive, vb_l'), e')
     | Nonrecursive -> vbs_to_nonrec_letin vb_l' e')
  | Exp_function (pt, e') ->
    let* fun_body = to_me_expr e' in
    let* potential_arg, me_potential_bind =
      let+ new_name = get_uniq_name in
      Id_name new_name, me_name new_name
    in
    let* alt_pat = elim_pattern me_potential_bind pt in
    let ( |-> ) name body = return @@ MExp_function (name, body) in
    (match alt_pat with
     | Var (SBind (arg, _)) | Any (SBind (arg, _)) -> arg |-> fun_body
     | Const cond ->
       let fun_body' = ite_with_fail ~cond ~then_br:fun_body in
       potential_arg |-> fun_body'
     | Compound (cond_opt, sbinds) ->
       let fun_body' =
         let then_f = sbinds_to_nonrec_letin sbinds in
         let then_br = then_f fun_body in
         match cond_opt with
         | Some cond -> ite_with_fail ~cond ~then_br
         | None -> then_br
       in
       potential_arg |-> fun_body')
  | Exp_match (main_e, branches) ->
    let* e_for_match' = to_me_expr main_e in
    let* bind_nm, me_bind_nm =
      let+ new_name = get_uniq_name in
      Id_name new_name, me_name new_name
    in
    let+ f_match' =
      fold_left_t
        branches
        ~init:(return (me_nonrec_letin bind_nm e_for_match'))
        ~f:(fun acc_f (pt, br_e) ->
          let* alt_pt = elim_pattern me_bind_nm pt in
          let+ br_me = to_me_expr br_e in
          match alt_pt with
          | Any _ -> fun _ -> acc_f br_me
          | Var (SBind (name, bind_body)) ->
            fun _ -> acc_f @@ me_nonrec_letin name bind_body br_me
          | Const cond -> fun else_br -> acc_f @@ ite ~cond ~then_br:br_me ~else_br
          | Compound (cond_opt, sbinds) ->
            let me_particial = sbinds_to_nonrec_letin sbinds in
            (match cond_opt with
             | Some cond ->
               fun else_br -> acc_f @@ ite ~cond ~then_br:(me_particial br_me) ~else_br
             | None -> fun _ -> acc_f @@ me_particial br_me))
    in
    f_match' fail_app
;;

let to_me_struct_item = function
  | Str_eval e ->
    let* new_uniq_bind_nm = get_uniq_name in
    let m_vb_pat = Id_name new_uniq_bind_nm in
    let+ m_vb_expr = to_me_expr e in
    [ MDecl (Nonrecursive, [ { m_vb_pat; m_vb_expr } ]) ]
  | Str_value (Decl (r_flag, vb_l)) ->
    let+ vb_l' = elim_vb'list to_me_expr vb_l in
    (match r_flag with
     | Recursive -> [ MDecl (Recursive, vb_l') ]
     | Nonrecursive -> List.map (fun vb -> MDecl (Nonrecursive, [ vb ])) vb_l')
;;

let to_me_program prog =
  let+ m_decls = mapt prog to_me_struct_item in
  List.concat m_decls
;;

let eliminate_match_in_program prog =
  let open Common.Errors in
  match run (to_me_program prog) with
  | _, Ok me_decl'list -> Result.Ok me_decl'list
  | _, Error msg -> Result.Error (illegal_state msg)
;;

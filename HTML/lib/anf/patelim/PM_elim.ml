open AstLib.Ast
open Elim_utils
open R
open Env
open IR

let get_typ_of_pat = function
  | PConstraint (_, typ) -> Some typ
  | _ -> None
;;

let rec is_pattern_suitable = function
  | PId _, _ -> return @@ econst (CBool true)
  | PConst x, e -> return @@ ebin_op Eq (econst x) e
  | PList (p1, p2), e ->
    let is_not_empty = ebin_op Neq (econst CNil) e in
    let* head = RuntimeEnv.apply RuntimeEnv.get_head e in
    let* tl = RuntimeEnv.apply RuntimeEnv.get_tl e in
    let* head_res = is_pattern_suitable (p1, head) in
    let* tl_res = is_pattern_suitable (p2, tl) in
    return @@ ebin_op And (ebin_op And is_not_empty head_res) tl_res
  | PTuple (p1, p2, ps), e ->
    let all_pats = p1 :: p2 :: ps in
    let check_nth n pat_n =
      let* nth_el =
        RuntimeEnv.apply
          RuntimeEnv.get_nth
          (e_typed ~typ:(get_typ_of_pat pat_n) (etuple (econst (CInt n)) e []))
      in
      is_pattern_suitable (pat_n, nth_el)
    in
    let* res, _ =
      List.fold_left
        (fun acc pat ->
          let* res, i = acc in
          let* i_check = check_nth i pat in
          let res = ebin_op And res i_check in
          return (res, i + 1))
        (return @@ (econst (CBool true), 0))
        all_pats
    in
    return res
  | PConstraint (pat, typ), e -> is_pattern_suitable (pat, e_typed ~typ:(Some typ) e)
;;

let rec get_pattern_vars = function
  | PConst _, _ | PId "_", _ -> return empty
  | PId id, e -> return @@ extend empty id e
  | PList (p1, p2), e ->
    let* head = RuntimeEnv.apply RuntimeEnv.get_head e in
    let* tl = RuntimeEnv.apply RuntimeEnv.get_tl e in
    let* p1_map = get_pattern_vars (p1, head) in
    let* p2_map = get_pattern_vars (p2, tl) in
    Env.merge p1_map p2_map
  | PTuple (p1, p2, ps), e ->
    let all_pats = p1 :: p2 :: ps in
    let* res, _ =
      List.fold_left
        (fun acc x ->
          let* acc, i = acc in
          let* nth_el =
            RuntimeEnv.apply RuntimeEnv.get_nth (etuple (econst (CInt i)) e [])
          in
          let* pattern_vars = get_pattern_vars (x, nth_el) in
          let* acc' = merge acc pattern_vars in
          return (acc', i + 1))
        (return (empty, 0))
        all_pats
    in
    return res
  | PConstraint (pat, typ), e -> get_pattern_vars (pat, e_typed ~typ:(Some typ) e)
;;

let pm_elim =
  let rec helper = function
    | (pat, e_res) :: tl, e ->
      let* cond = is_pattern_suitable (pat, e) in
      let* pattern_vars = get_pattern_vars (pat, e) in
      let* res_expr =
        Base.Map.fold_right pattern_vars ~init:(return e_res) ~f:(fun ~key ~data acc ->
          let* acc = acc in
          let pat = pid key in
          let pat =
            match data with
            | NoPMEConstraint (_, typ) -> p_typed ~typ:(Some typ) pat
            | _ -> pat
          in
          let decl = dlet Not_recursive (pop_pat pat, data) in
          return @@ eclsr decl acc)
      in
      let* res_tl = helper (tl, e) in
      (match cond with
       | NoPMEConst (CBool true) -> return @@ res_expr
       | _ -> return @@ eif cond res_expr res_tl)
    | [], _ -> RuntimeEnv.apply RuntimeEnv.not_exhaustive_pm (econst CUnit)
  in
  helper
;;

let rec pm_elim_expr expr =
  let rec helper = function
    | EConst x -> return @@ econst x
    | EId x -> return @@ eid x
    | EIf (i, t, e) ->
      let* i_res = helper i in
      let* t_res = helper t in
      let* e_res = helper e in
      return @@ eif i_res t_res e_res
    | EApp (e1, e2) ->
      let* e1_res = helper e1 in
      let* e2_res = helper e2 in
      return @@ eapp e1_res e2_res
    | EFun (pat, e) ->
      let* e_res = helper e in
      return @@ efun pat e_res
    | EClsr (decl, expr) ->
      let* no_pm_decl = pm_elim_decl decl in
      let* expr_res = helper expr in
      return @@ eclsr no_pm_decl expr_res
    | EList (e1, e2) ->
      let* e1_res = helper e1 in
      let* e2_res = helper e2 in
      return @@ elist e1_res e2_res
    | ETuple (e1, e2, els) ->
      let* e1_res = helper e1 in
      let* e2_res = helper e2 in
      let* no_pm_els = map pm_elim_expr els in
      return @@ etuple e1_res e2_res no_pm_els
    | EConstraint (e, typ) ->
      let* e_res = helper e in
      return @@ e_typed ~typ:(Some typ) e_res
    | EMatch (e, br, brs) ->
      let* e_res = helper e in
      let* fresh = fresh in
      let evaluated_name = "EVALUATED_" ^ string_of_int fresh in
      let evaluated_pat = pop_pat (pid evaluated_name) in
      let evaluated_e = eid (ident_of_definable (ident_letters evaluated_name)) in
      let* no_pm_els =
        map
          (fun (pat, e) ->
            let* no_pm_el = pm_elim_expr e in
            return (pat, no_pm_el))
          (br :: brs)
      in
      let* res = pm_elim (no_pm_els, evaluated_e) in
      return @@ eclsr (dlet Not_recursive (evaluated_pat, e_res)) res
  in
  helper expr

and pm_elim_decl decl =
  let helper decl =
    match decl with
    | DLet (rec_flag, (pat, expr)) ->
      let* no_pm_expr = pm_elim_expr expr in
      return @@ NoPMDLet (rec_flag, (pat, no_pm_expr))
    | DLetMut (rec_flag, (pat, expr), (pat1, expr1), tl) ->
      let applied = tl in
      let* res =
        map
          (fun (pat, e) ->
            let* e = pm_elim_expr e in
            return (pat, e))
          applied
      in
      let* expr = pm_elim_expr expr in
      let* expr1 = pm_elim_expr expr1 in
      return @@ NoPMDLetMut (rec_flag, (pat, expr), (pat1, expr1), res)
  in
  helper decl
;;

let pm_elim_decls decls =
  run
    (map
       (fun x -> pm_elim_decl x >>| IR_utils.transform_expr_in_decl IR_utils.optimize)
       decls)
;;

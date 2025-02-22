(* Elimination of patterns with consts *)
open AstLib.Ast
open IR
open Elim_utils
open R

let is_pc (p : pattern) =
  match p with
  | PConst _ -> return true
  | _ -> return false
;;

let create_assertion_or_error cond error = eif cond (econst CUnit) error

let rec pc_elim_expr expr : expr_no_pm t =
  match expr with
  | NoPMEConst _ -> return expr
  | NoPMEId _ -> return expr
  | NoPMEApp (f, args) ->
    let* f' = pc_elim_expr f in
    let* args' = pc_elim_expr args in
    return (eapp f' args')
  | NoPMEFun (pat, e) ->
    let* pat', e' =
      match pat with
      | PConst c ->
        let* fresh_var = fresh in
        let pat_id = "P" ^ string_of_int fresh_var in
        let pat = PId pat_id in
        let* assertion_in_let =
          let* apply_not_exhaustive_pm = RuntimeUtils.apply_not_exhaustive_pm () in
          let assertion =
            create_assertion_or_error
              (ebin_op Eq (eid (ident_of_definable (ident_letters pat_id))) (econst c))
              apply_not_exhaustive_pm
          in
          let* e' = pc_elim_expr e in
          return @@ eclsr (dlet Not_recursive (POpPat (PId "_"), assertion)) e'
        in
        return @@ (pat, assertion_in_let)
      | _ ->
        let* e' = pc_elim_expr e in
        return (pat, e')
    in
    return (efun pat' e')
  | NoPMEIf (e1, e2, e3) ->
    let* e1' = pc_elim_expr e1 in
    let* e2' = pc_elim_expr e2 in
    let* e3' = pc_elim_expr e3 in
    return (eif e1' e2' e3')
  | NoPMEList (hd, tl) ->
    let* hd' = pc_elim_expr hd in
    let* tl' = pc_elim_expr tl in
    return (elist hd' tl')
  | NoPMETuple (e1, e2, l) ->
    let* e1' = pc_elim_expr e1 in
    let* e2' = pc_elim_expr e2 in
    let* l' = map pc_elim_expr l in
    return (etuple e1' e2' l')
  | NoPMEClsr (d, e) ->
    let* d' = pc_elim_decl d in
    let* e' = pc_elim_expr e in
    return (eclsr d' e')
  | NoPMEBinOp (bop, e1, e2) ->
    let* e1' = pc_elim_expr e1 in
    let* e2' = pc_elim_expr e2 in
    return (ebin_op bop e1' e2')
  | NoPMEConstraint (e, typ) ->
    let* e' = pc_elim_expr e in
    return (e_typed ~typ:(Some typ) e')

and pc_elim_decl decl : decl_no_pm t =
  match decl with
  | NoPMDLet (rec_flag, (pat, e)) ->
    let* e' = pc_elim_expr e in
    (match pat with
     | POpPat (PConst c) ->
       let* fresh = fresh in
       let evaluated_name = RuntimeUtils.create_var_for_eval (string_of_int fresh) in
       let evaluated_pat, evaluated_e =
         RuntimeUtils.create_pop_and_expr_for_eval evaluated_name
       in
       let decl = dlet Not_recursive (evaluated_pat, e') in
       let cond = ebin_op Eq evaluated_e (econst c) in
       let* error = RuntimeUtils.apply_not_exhaustive_pm () in
       let assertion = create_assertion_or_error cond error in
       let clsr = eclsr decl assertion in
       return (NoPMDLet (rec_flag, (POpPat (PId "_"), clsr)))
     | _ -> return (NoPMDLet (rec_flag, (pat, e'))))
  | NoPMDLetMut (rec_flag, let_body1, let_body2, tl) ->
    (* convert decls to NoPMDLet, and then apply pc_elim_desk for each decl *)
    let create_decl = dlet rec_flag in
    let* let_body1' = pc_elim_decl @@ dlet rec_flag let_body1 in
    let* let_body2' = pc_elim_decl @@ dlet rec_flag let_body2 in
    let all = List.map create_decl tl in
    let* all = map pc_elim_decl all in
    let revert = function
      | NoPMDLet (_, x) -> return x
      | _ -> fail "Unexpected value: NoPMDLet became NoPMDLetMut?!"
    in
    let* let_body1'' = revert let_body1' in
    let* let_body2'' = revert let_body2' in
    let* reverted = map revert all in
    return (NoPMDLetMut (rec_flag, let_body1'', let_body2'', reverted))
;;

let pc_elim_decls prog =
  let* no_pm = PM_elim.pm_elim_decls prog in
  map pc_elim_decl no_pm
;;

let pc_elim prog = run (pc_elim_decls prog)

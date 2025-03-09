open AstLib.Ast
open Common.Ident_utils

let get_new_id new_name = function
  | IdentOfDefinable (IdentLetters _) -> IdentOfDefinable (IdentLetters new_name)
  | IdentOfDefinable (IdentOp _) -> IdentOfDefinable (IdentOp new_name)
  | a -> a
;;

let revert_str =
  String.map (function
    | '.' -> '_'
    | a -> a)
;;

let rec revert_pat = function
  | PConst _ as p -> p
  | PId id -> pid @@ revert_str id
  | PList (hd, tl) -> plist (revert_pat hd) (revert_pat tl)
  | PTuple (p1, p2, ps) -> ptuple (revert_pat p1) (revert_pat p2) (List.map revert_pat ps)
  | PConstraint (p, typ) -> p_typed ~typ:(Some typ) (revert_pat p)
;;

let rec revert_expr =
  let rec helper = function
    | EId id -> eid (get_new_id (revert_str @@ ident_to_string id) id)
    | EConst _ as e -> e
    | EApp (e1, e2) -> eapp (helper e1) (helper e2)
    | EIf (e1, e2, e3) -> eif (helper e1) (helper e2) (helper e3)
    | EFun (pat, e) -> efun (revert_pat pat) (helper e)
    | EList (e1, e2) -> elist (revert_expr e1) (revert_expr e2)
    | EMatch (e, case1, cases) ->
      let revert_branch (pat, expr) = revert_pat pat, helper expr in
      ematch (helper e) (revert_branch case1) (List.map revert_branch cases)
    | ETuple (e1, e2, es) -> etuple (helper e1) (helper e2) (List.map revert_expr es)
    | EClsr (decl, e) -> eclsr (revert_decl decl) (helper e)
    | EConstraint (e, typ) -> e_typed ~typ:(Some typ) (helper e)
  in
  helper

and revert_decl = function
  | DLet (rec_flag, (pop, e)) -> dlet rec_flag (revert_pop pop, revert_expr e)
  | DLetMut (rec_flag, lb1, lb2, lbs) ->
    dletmut rec_flag (revert_lb lb1) (revert_lb lb2) (List.map revert_lb lbs)

and revert_pop = function
  | POpPat pat -> pop_pat (revert_pat pat)
  | POpOp id -> pop_op (revert_str id)
  | POrOpConstraint (pop, typ) -> pop_typed ~typ:(Some typ) (revert_pop pop)

and revert_lb (pop, e) = revert_pop pop, revert_expr e

open Ast
open Base

(* let find_free_vars expr = 
  let fun_helper set = function
  | EFun (PVar x, e)
  | *)
let find_free_vars expr =
  let rec fun_helper set = function
    | EFun (PVar x, e) -> fun_helper (Set.add set x) e
    | EFun (_, e) -> fun_helper set e
    | _ -> set
  in
  (* let see_binding_list list_ = List.map *)
  (* | Rec -> let pattern = Set.remove sub_env pat *)
  (* | Nonrec -> sub_env) *)
    (* fun list_ -> List.map list_ helper in  *)

  let rec helper = function
    | EConst _ -> Set.empty (module String)
    | EVar x -> Set.singleton (module String) x
    (* | EBinOp (_, e1, e2) -> Set.union (helper e1) (helper e2) *)
    | EIf (e1, e2, e3) -> Set.union (Set.union (helper e1) (helper e2)) (helper e3)
    | EApply (e1, e2) -> Set.union (helper e1) (helper e2)
    | EFun (name, e) ->
      (match name with
       | PVar name -> Set.remove (helper e) name
       | _ -> helper e)
    | ELet (rec_flag, [ (PVar pattern_, e1) ], e2) ->
      let free1 = helper e1 in
      let free1' =
        (match rec_flag with
        | Rec -> Set.remove free1 pattern_
        | Nonrec -> free1)
      in
      let e1_pat = fun_helper (Set.empty (module String)) e1 in
      let free2 = Set.diff (helper e2) e1_pat in
      let free2' = Set.remove free2 pattern_ in
      Set.union free1' free2'
  in
  helper expr
;;
let closure_conv g_env decl =
  let rec expr_closure loc_env g_env = function
    | EConst x -> EConst x
    | EVar x as orig -> 
      (match Map.find loc_env x with
       | Some free ->
         let ids = List.map (Set.to_list free) ~f:(fun x -> EVar x) in
         constr_apply orig ids
       | None -> orig)
    | EIf (e1, e2, e3) ->
      let e1' = expr_closure loc_env g_env e1 in
      let e2' = expr_closure loc_env g_env e2 in
      let e3' = expr_closure loc_env g_env e3 in
        EIf (e1', e2', e3')
    | EApply(e1, e2) ->
      let e1' = expr_closure loc_env g_env e1 in
      let e2' = expr_closure loc_env g_env e2 in
      EApply (e1', e2')
    | EFun (x, _) as orig ->
      let s = find_free_vars orig in
      let s' = Set.diff s g_env in
      let e' = fun_helper loc_env g_env orig in
      (match x with
       | PVar _ ->
         let fun_fold =
           constr_fun (List.map (Set.to_list s') ~f:(fun x -> PVar x)) e'
         in
         constr_apply fun_fold (List.map (Set.to_list s') ~f:(fun x -> EVar x))
       | _ -> e')

    | ELet (rec_flag, [PVar x, (EFun (_, _) as e1)], e2) as orig ->
      let free = find_free_vars orig in
      let free' = Set.diff free g_env in
      let e1' = fun_helper loc_env g_env e1 in
      let e1_closure =
        constr_fun (List.map (Set.to_list free') ~f:(fun x -> PVar x)) e1'
      in
      let local_env' = Map.set loc_env ~key:x ~data:free' in
      let e2' = expr_closure local_env' (Set.add g_env x) e2 in
      ELet (rec_flag, [PVar(x), e1_closure], e2')

    and fun_helper local_env global_env = function
    | EFun ((PVar _ as orig), e) ->
      let e' = fun_helper local_env global_env e in
      constr_fun [ orig ] e'
    | EFun (p, e) ->
      let e' = fun_helper local_env global_env e in
      constr_fun [ p ] e'
    | expr -> expr_closure local_env global_env expr 
  
  in let decl_closure global_env = function
    | ELet (rec_flag, [ (PVar id, e1) ], e) ->
      let global_env' = 
        (match rec_flag with
        | Rec -> Set.add g_env id
        | Nonrec -> g_env) in
        (* if is_rec then Set.add global_env id else global_env in *)
      let e' = fun_helper (Map.empty (module String)) global_env' e in
      ELet (rec_flag, [PVar id, e1], e')
  in
  decl_closure g_env decl
;;
(* | ELet (rec_flag, blist, expr) -> let blist_process blist =  *)

(* let free1 =  *)
(* match rec_flag with *)
(* | Rec -> Set.remove(helper blist) *)
(* | Nonrec -> *)
(* let free1' = if b then Set.remove free1 x else free1 in *)
(* let e1_pat = efun_helper (Set.empty (module String)) e1 in *)
(* let free2 = Set.diff (helper e2) e1_pat in *)
(* let free2' = Set.remove free2 x in *)
(* Set.union free1' free2' *)
(* in
  helper expr *)
(* and  *)
(* numbers = [1; 2; 3; 4; 5];; *)
(* let doubled_numbers = List.map (fun x -> x * 2) numbers;; *)
(* blist_process rec_flag blist expr helper fun_helper = List.map blist 
  (
    let free1 pat blist_expr = (helper blist_expr, blist_expr, pat) 
      in let free1' = match rec_flag with
        | Rec -> Set.remove
        | Nonrec -> 
  in free_) *)
(* (match rec_flag with
      | Rec -> Set.remove (
          match free_ with
          | (free1_res, blist_expr, pat) -> free1_res pat
          )
    )) *)
(* | Nonrec -> free1), blist_expr *)
(* in let e1_pat = fun_helper (Set.empty (module String)) blist_expr in
      let free2 = Set.diff (helper expr) e1_pat in
      let free2' = Set.remove free2 x in
      Set.union free1' free2'  *)

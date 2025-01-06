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
   let rec helper = 
    (* fun list_ -> List.map list_ helper in  *)
      function
    | EConst _ -> Set.empty (module String)
    | EVar x -> Set.singleton (module String) x
    (* | EBinOp (_, e1, e2) -> Set.union (helper e1) (helper e2) *)
    | EIf (e1, e2, e3) -> Set.union (Set.union (helper e1) (helper e2)) (helper e3)
    | EApply (e1, e2) -> Set.union (helper e1) (helper e2)
    | EFun (name, e) ->
      (match name with
    | PVar name -> Set.remove (helper e) name
       | _ -> helper e)
    | ELet (rec_flag, blist, expr) ->
      let free1 = match rec_flag with
      | Rec -> Set.remove(helper blist)
      | Nonrec ->
      (* let free1' = if b then Set.remove free1 x else free1 in *)
      (* let e1_pat = efun_helper (Set.empty (module String)) e1 in *)
      (* let free2 = Set.diff (helper e2) e1_pat in *)
      (* let free2' = Set.remove free2 x in *)
      (* Set.union free1' free2' *)
  in
  helper expr
;;
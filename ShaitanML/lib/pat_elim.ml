open Ast
open Pat_elim_ast
open Base
open Common

let const_to_ast = function
  | PECBool b -> CBool b
  | PECint i -> CInt i
  | PECUnit -> CUnit
  | PECNil -> CNil
  | PECString s -> CString s
;;

let to_pattern name = PVar name

let rec expr_to_ast = function
  | PEEConst c -> EConst (const_to_ast c)
  | PEEApp (l, r) -> EApply (expr_to_ast l, expr_to_ast r)
  | PEECons (l, r) -> ECons (expr_to_ast l, expr_to_ast r)
  | PEEFun (args, body) ->
    let pats = List.map args ~f:(fun x -> PVar x) in
    List.fold_right pats ~init:(expr_to_ast body) ~f:(fun pat acc -> EFun (pat, acc))
  | PEEVar v -> EVar v
  | PEEIf (e1, e2, e3) -> EIf (expr_to_ast e1, expr_to_ast e2, expr_to_ast e3)
  | PEELet (decl, e) ->
    (match decl with
     | PENonrec (name, e1) ->
       ELet (Nonrec, (PVar name, expr_to_ast e1), expr_to_ast e)
     | PERec [ (name, e1) ] -> ELet (Rec, (PVar name, expr_to_ast e1), expr_to_ast e))
  | PEETuple e_list ->
    let e_list = List.map e_list ~f:expr_to_ast in
    ETuple e_list

and decl_body_to_ast (name, e) = to_pattern name, expr_to_ast e

and decl_to_ast = function
  | PENonrec (name, e) -> SValue (Nonrec, [ decl_body_to_ast (name, e) ])
  | PERec decls -> SValue (Rec, List.map decls ~f:decl_body_to_ast)
;;

let convert_program p = List.map p ~f:decl_to_ast
let empty_bindings = Map.empty (module String)
let get_name i = "a" ^ Int.to_string i

let convert_const =
  let open Ast in
  function
  | CBool b -> PECBool b
  | CInt i -> PECint i
  | CNil -> PECNil
  | CUnit -> PECUnit
  | CString s -> PECString s
;;

type unpack =
  | Tuple of int
  | List_hd
  | List_tl
  | Value

let unpack_expr e = function
  | Tuple i -> PEEApp (PEEApp (PEEVar "tuple_element", e), PEEConst (PECint i))
  | List_hd -> PEEApp (PEEVar "list_head", e)
  | List_tl -> PEEApp (PEEVar "list_tail", e)
  | Value -> e
;;

let pat_eqs expr pat =
  let rec get_min_lenght l = function
    | Ast.PCons (_, r) -> get_min_lenght (l + 1) r
    | _ -> l
  in
  let rec helper add_list cur = function
    | Ast.PConstraint (p, _) -> helper add_list cur p
    | PConst c ->
      (match c with
       | CUnit -> []
       | _ -> [ PEEApp (PEEApp (PEEVar "=", cur), PEEConst (convert_const c)) ])
    | PTuple pl ->
      let t = List.mapi pl ~f:(fun i p -> helper true (unpack_expr cur (Tuple i)) p) in
      List.concat t
    | PCons (l, r) ->
      let min_length = get_min_lenght 0 r in
      let list_length = PEEApp (PEEVar "list_len", cur) in
      let check =
        PEEApp (PEEApp (PEEVar ">", list_length), PEEConst (PECint min_length))
      in
      let l = helper true (unpack_expr cur List_hd) l in
      let r = helper false (unpack_expr cur List_tl) r in
      if add_list then (check :: l) @ r else l @ r
    | _ -> []
  in
  helper true expr pat
;;

let pat_decls expr pat =
  let rec helper name = function
    | Ast.PConstraint (p, _) -> helper name p
    | PCons (l, r) ->
      (match helper name l with
       | _ :: _ as lst -> List_hd :: lst
       | _ -> List_tl :: helper name r)
    | PTuple pl ->
      let t = List.map pl ~f:(helper name) in
      (match List.findi t ~f:(fun _ a -> not @@ List.is_empty a) with
       | Some (i, lst) -> Tuple i :: lst
       | None -> [])
    | PVar v when String.equal v name -> [ Value ]
    | _ -> []
  in
  let create_expr name =
    List.fold_left (helper name pat) ~init:expr ~f:(fun acc unpack ->
      unpack_expr acc unpack)
  in
  let names = get_idents pat in
  List.map (StrSet.to_list names) ~f:(fun name -> PENonrec (name, create_expr name))
;;

let create_if checks e1 e2 =
  let cond =
    List.fold (List.tl_exn checks) ~init:(List.hd_exn checks) ~f:(fun acc a ->
      PEEApp (PEEApp (PEEVar "&&", acc), a))
  in
  PEEIf (cond, e1, e2)
;;

let create_case to_match pat case_expr not_match_expr =
  let checks = pat_eqs to_match pat in
  let decls = pat_decls to_match pat in
  let let_expr =
    List.fold_right decls ~init:case_expr ~f:(fun decl_body acc ->
      PEELet (decl_body, acc))
  in
  if List.is_empty checks then let_expr else create_if checks let_expr not_match_expr
;;

let match_failure = PEEVar "fail_match"

open Common.MonadCounter

let rec pe_expr =
  let open Ast in
  function
  | EConstraint (e, _) -> pe_expr e
  | EConst c -> return @@ PEEConst (convert_const c)
  | EVar v -> return @@ PEEVar v
  | EApply (e1, e2) ->
    let* e1 = pe_expr e1 in
    let* e2 = pe_expr e2 in
    return @@ PEEApp (e1, e2)
  | EIf (e1, e2, e3) ->
    let* e1 = pe_expr e1 in
    let* e2 = pe_expr e2 in
    let* e3 = pe_expr e3 in
    return @@ PEEIf (e1, e2, e3)
  | ECons (e1, e2) ->
    let* e1 = pe_expr e1 in
    let* e2 = pe_expr e2 in
    return @@ PEECons (e1, e2)
  | ETuple e_list ->
    let* e_list = RList.map e_list ~f:(fun e -> pe_expr e) in
    return @@ PEETuple e_list
  | EFun (p, exp) ->
    let rec extract_body = function
      | EFun (_, e) -> extract_body e
      | e -> e
    in
    let body = extract_body exp in
    let rec extract_args = function
      | EFun (p, e) -> p :: extract_args e
      | _ -> []
    in
    let other = extract_args exp in
    let last_args = p :: other in
    let f1 (new_args, args_to_match, pat_list) arg =
      match arg with
      | PVar v -> return (v :: new_args, args_to_match, pat_list)
      | PConst CUnit -> return ("()" :: new_args, args_to_match, pat_list)
      | _ ->
        let* fresh_name = fresh >>| get_name in
        return (fresh_name :: new_args, fresh_name :: args_to_match, arg :: pat_list)
    in
    let* new_args, args_to_match, pat_list =
      RList.fold_left last_args ~init:(return ([], [], [])) ~f:f1
    in
    let new_args = List.rev new_args in
    let args_to_match = List.rev args_to_match in
    let pat_list = List.rev pat_list in
    let* new_body = pe_expr body in
    (match List.length args_to_match with
     | 0 -> return @@ PEEFun (new_args, new_body)
     | 1 ->
       let pat = List.hd_exn pat_list in
       let to_match = PEEVar (List.hd_exn args_to_match) in
       let case_expr = create_case to_match pat new_body match_failure in
       return @@ PEEFun (new_args, case_expr)
     | _ ->
       let pat = PTuple pat_list in
       let to_match =
         let vals = List.map args_to_match ~f:(fun a -> PEEVar a) in
         PEETuple vals
       in
       let* fresh_name = fresh >>| get_name in
       let case_expr = create_case (PEEVar fresh_name) pat new_body match_failure in
       return @@ PEEFun (new_args, PEELet (PENonrec (fresh_name, to_match), case_expr)))
  | EMatch (e_last, case_list) ->
    let* e = pe_expr e_last in
    (match e_last with
     | EVar _ | EConst _ -> pe_match e case_list
     | _ ->
       let* fresh_name = fresh >>| get_name in
       let* e_match = pe_match (PEEVar fresh_name) case_list in
       return @@ PEELet (PENonrec (fresh_name, e), e_match))
  | ELet (Nonrec, (pat, e1), e2) ->
    let* e1 = pe_expr e1 in
    let* e2 = pe_expr e2 in
    (match pat with
     | PVar name -> return @@ PEELet (PENonrec (name, e1), e2)
     | PConst CUnit -> return @@ PEELet (PENonrec ("()", e1), e2)
     | _ ->
       (match e1 with
        | PEEVar _ ->
          let case_expr = create_case e1 pat e2 match_failure in
          return case_expr
        | _ ->
          let* fresh_name = fresh >>| get_name in
          let case_expr = create_case (PEEVar fresh_name) pat e2 match_failure in
          return @@ PEELet (PENonrec (fresh_name, e1), case_expr)))
  | ELet (Rec, (pat, e1), e2) ->
    let* decl = pe_decl [ pat, e1 ] in
    let* e = pe_expr e2 in
    return @@ PEELet (decl, e)

and pe_match to_match = function
  | (p, e) :: tl ->
    let checks = pat_eqs to_match p in
    let decls = pat_decls to_match p in
    let* e = pe_expr e in
    let let_in = List.fold_right decls ~init:e ~f:(fun d acc -> PEELet (d, acc)) in
    if List.is_empty checks
    then return let_in
    else
      let* match_e = pe_match to_match tl in
      return @@ create_if checks let_in match_e
  | _ -> return @@ PEEVar "fail_match"

and pe_decl decl_list =
  let f1 (pat, e) =
    let* e = pe_expr e in
    return
      (match pat with
       | Ast.PVar v -> v, e
       | PConst CUnit -> "()", e
       | _ -> "", e)
  in
  let* new_decls = RList.map decl_list ~f:f1 in
  return @@ PERec new_decls
;;

let pe_str_item = function
  | Ast.SValue (Nonrec, [ (pat, e) ]) ->
    let* e = pe_expr e in
    (match pat with
     | PVar name -> return [ PENonrec (name, e) ]
     | PConst CUnit -> return [ PENonrec ("()", e) ]
     | pat ->
       let* fresh_name = fresh >>| get_name in
       let to_unpack = PEEVar fresh_name in
       let checks = pat_eqs to_unpack pat in
       let decls = pat_decls to_unpack pat in
       if List.is_empty checks
       then return (PENonrec (fresh_name, e) :: decls)
       else (
         let ite = create_if checks (PEEConst PECUnit) (PEEVar "fail_match") in
         return (PENonrec (fresh_name, e) :: PENonrec ("()", ite) :: decls)))
  | Ast.SValue (Rec, decl_list) ->
    let* decl_list = pe_decl decl_list in
    return [ decl_list ]
;;

let pe_structure program =
  let rec helper = function
    | [] -> return []
    | hd :: tl ->
      let* hd = pe_str_item hd in
      let* tl = helper tl in
      return @@ hd @ tl
  in
  helper program
;;

let run p = run (pe_structure p) (NamesHolder.create p) 0

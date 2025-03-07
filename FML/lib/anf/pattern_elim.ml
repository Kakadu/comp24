(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Pe_ast
open Common

let const_to_str = function
  | Pe_CBool b -> if b then "true" else "false"
  | Pe_Cint i -> Format.sprintf "%i" i
;;

let rec expr_to_str = function
  | Pe_EUnit -> "()"
  | Pe_ENill -> "[]"
  | Pe_EIdentifier a -> a
  | Pe_EConst c -> const_to_str c
  | Pe_EIf (e1, e2, e3) ->
    Format.sprintf
      "if %s\nthen %s\nelse %s"
      (expr_to_str e1)
      (expr_to_str e2)
      (expr_to_str e3)
  | Pe_EFun (args, e) ->
    Format.sprintf
      "(fun%s -> %s)"
      (List.fold_left (fun acc name -> acc ^ " " ^ name) "" args)
      (expr_to_str e)
  | Pe_EApp (e1, e2) -> Format.sprintf "(%s %s)" (expr_to_str e1) (expr_to_str e2)
  | Pe_ELet (NoRec, name, e1, e2) ->
    Format.sprintf "let %s = %s in\n%s" name (expr_to_str e1) (expr_to_str e2)
  | Pe_ELet (Rec, name1, e1, e2) ->
    Format.sprintf "let rec %s = %s in\n%s" name1 (expr_to_str e1) (expr_to_str e2)
  | Pe_ECons (e1, e2) -> Format.sprintf "(%s::%s)" (expr_to_str e1) (expr_to_str e2)
  | Pe_ETuple e_list ->
    Format.sprintf
      "(%s)"
      (expr_to_str (List.hd e_list)
       ^ List.fold_left
           (fun acc e -> acc ^ Format.sprintf ", %s" (expr_to_str e))
           ""
           (List.tl e_list))
;;

let decl_to_str = function
  | Pe_Nonrec decl_list ->
    (match decl_list with
     | [] -> ""
     | (name, e) :: tl ->
       Format.sprintf "let %s = %s" name (expr_to_str e)
       ^ List.fold_left
           (fun acc (name, e) ->
             acc ^ Format.sprintf "\nlet %s = %s" name (expr_to_str e))
           ""
           tl)
  | Pe_Rec decl_list ->
    (match decl_list with
     | [] -> ""
     | (name, e) :: tl ->
       Format.sprintf "let rec %s = %s" name (expr_to_str e)
       ^ List.fold_left
           (fun acc (name, e) ->
             acc ^ Format.sprintf "\nand %s = %s" name (expr_to_str e))
           ""
           tl)
;;

let pp_pe_structure ppf p =
  let len = List.length p in
  List.iteri
    (fun i a ->
      if i = len - 1
      then Format.fprintf ppf "%s" (decl_to_str a)
      else Format.fprintf ppf "%s\n\n" (decl_to_str a))
    p
;;

type value_to_get =
  | Tuple of int
  | Cons_head
  | Cons_tail
  | Other

let get_element e = function
  | Tuple i -> Pe_EApp (Pe_EApp (Pe_EIdentifier "tuple_element", e), Pe_EConst (Pe_Cint i))
  | Cons_head -> Pe_EApp (Pe_EIdentifier "list_head", e)
  | Cons_tail -> Pe_EApp (Pe_EIdentifier "list_tail", e)
  | Other -> e
;;

let const_to_peconst const =
  let pe_const =
    match const with
    | CInt i -> Pe_Cint i
    | CBool b -> Pe_CBool b
  in
  Pe_EConst pe_const
;;

open Base
open MonadCounter

(* let check_pattern expr pat =
  let rec helper expr = function
    | PConstraint (p, _) -> helper expr p
    | PConst c -> [ make_apply "( = )" expr (const_to_peconst c) ]
    | PTuple pl ->
      List.concat @@ List.mapi pl ~f:(fun i p -> helper (get_element expr (Tuple i)) p)
    | PCons (l, r) ->
      let l = helper (get_element expr Cons_head) l in
      
      let r = helper (get_element expr Cons_tail) r in
      l @ r
    | PNill -> [ Pe_EApp (Pe_EIdentifier "is_empty", expr) ]
    | _ -> []
  in
  helper expr pat
;; *)

let check_pattern expr pat =
  let rec helper add expr = function
    | PConstraint (p, _) -> helper add expr p
    | PConst c -> [ make_apply "( = )" expr (const_to_peconst c) ]
    | PTuple pl ->
      List.concat
      @@ List.mapi pl ~f:(fun i p -> helper true (get_element expr (Tuple i)) p)
    | PCons (l, r) ->
      let check =
        Pe_EApp (Pe_EIdentifier "not", Pe_EApp (Pe_EIdentifier "is_empty", expr))
      in
      let l = helper true (get_element expr Cons_head) l in
      let r = helper false (get_element expr Cons_tail) r in
      if add then (check :: l) @ r else l @ r
    | PNill -> [ Pe_EApp (Pe_EIdentifier "is_empty", expr) ]
    | _ -> []
  in
  helper true expr pat
;;

let check_declaration expr pat =
  let rec helper name = function
    | PConstraint (p, _) -> helper name p
    | PCons (l, r) ->
      (match helper name l with
       | _ :: _ as lst -> Cons_head :: lst
       | _ -> Cons_tail :: helper name r)
    | PTuple pl ->
      let t = List.map pl ~f:(helper name) in
      (match List.findi t ~f:(fun _ a -> not @@ List.is_empty a) with
       | Some (i, lst) -> Tuple i :: lst
       | None -> [])
    | PIdentifier v when String.equal v name -> [ Other ]
    | _ -> []
  in
  let create_expr name =
    List.fold_left (helper name pat) ~init:expr ~f:(fun acc unpack ->
      get_element acc unpack)
  in
  let names = get_binds_pat pat in
  let decls = List.map (StrSet.to_list names) ~f:(fun name -> name, create_expr name) in
  Pe_Nonrec decls
;;

let make_case expr pat case_expr not_match_expr =
  let checks = check_pattern expr pat in
  let decl = check_declaration expr pat in
  let let_expr =
    match decl with
    | Pe_Nonrec decl_list ->
      List.fold_right decl_list ~init:case_expr ~f:(fun (name, value) acc ->
        Pe_ELet (NoRec, name, value, acc))
    | Pe_Rec decl_list ->
      List.fold_right decl_list ~init:case_expr ~f:(fun (name, value) acc ->
        Pe_ELet (Rec, name, value, acc))
  in
  if List.is_empty checks then let_expr else make_condition checks let_expr not_match_expr
;;

let rec pe_expr =
  let open Ast in
  function
  | EUnit -> return @@ Pe_EUnit
  | ENill -> return @@ Pe_ENill
  | EConstraint (e, _) -> pe_expr e
  | EConst c -> return @@ const_to_peconst c
  | EIdentifier v -> return @@ Pe_EIdentifier v
  | EApplication (e1, e2) ->
    let* e1 = pe_expr e1 in
    let* e2 = pe_expr e2 in
    return @@ Pe_EApp (e1, e2)
  | EIf (e1, e2, e3) ->
    let* e1 = pe_expr e1 in
    let* e2 = pe_expr e2 in
    let* e3 = pe_expr e3 in
    return @@ Pe_EIf (e1, e2, e3)
  | ECons (e1, e2) ->
    let* e1 = pe_expr e1 in
    let* e2 = pe_expr e2 in
    return @@ Pe_ECons (e1, e2)
  | ETuple e_list ->
    let* e_list = map e_list ~f:(fun e -> pe_expr e) in
    return @@ Pe_ETuple e_list
  | EFun (p, e) ->
    let rec extract_body = function
      | EFun (_, e) -> extract_body e
      | e -> e
    in
    let body = extract_body e in
    let rec extract_args = function
      | EFun (p, e) -> p :: extract_args e
      | _ -> []
    in
    let other = extract_args e in
    let last_args = p :: other in
    let f1 (new_args, args_to_match, pat_list) arg =
      match arg with
      | PIdentifier v when not (List.mem new_args v ~equal:String.equal) ->
        return (v :: new_args, args_to_match, pat_list)
      | _ ->
        let* fresh_name = fresh >>| get_id in
        return (fresh_name :: new_args, fresh_name :: args_to_match, arg :: pat_list)
    in
    let* new_args, args_to_match, pat_list =
      fold_left last_args ~init:(return ([], [], [])) ~f:f1
    in
    let new_args = List.rev new_args in
    let args_to_match = List.rev args_to_match in
    let pat_list = List.rev pat_list in
    let* new_body = pe_expr body in
    (match List.length args_to_match with
     | 0 -> return @@ Pe_EFun (new_args, new_body)
     | 1 ->
       let pat = List.hd_exn pat_list in
       let to_match = Pe_EIdentifier (List.hd_exn args_to_match) in
       let case_expr = make_case to_match pat new_body (Pe_EIdentifier "fail_match") in
       return @@ Pe_EFun (new_args, case_expr)
     | _ ->
       let pat = PTuple pat_list in
       let to_match =
         let vals = List.map args_to_match ~f:(fun a -> Pe_EIdentifier a) in
         Pe_ETuple vals
       in
       let* fresh_name = fresh >>| get_id in
       let case_expr =
         make_case (Pe_EIdentifier fresh_name) pat new_body (Pe_EIdentifier "fail_match")
       in
       return @@ Pe_EFun (new_args, Pe_ELet (NoRec, fresh_name, to_match, case_expr)))
  | EMatch (e_last, case_list) ->
    let* e = pe_expr e_last in
    (match e_last with
     | EIdentifier _ | EConst _ -> pe_match e case_list
     | _ ->
       let* fresh_name = fresh >>| get_id in
       let* e_match = pe_match (Pe_EIdentifier fresh_name) case_list in
       return @@ Pe_ELet (NoRec, fresh_name, e, e_match))
  | ELetIn (NoRec, pat, e1, e2) ->
    let* e1 = pe_expr e1 in
    let* e2 = pe_expr e2 in
    (match pat with
     | PIdentifier name -> return @@ Pe_ELet (NoRec, name, e1, e2)
     | PUnit -> return @@ Pe_ELet (NoRec, "()", e1, e2)
     | _ ->
       (match e1 with
        | Pe_EIdentifier _ ->
          let case_expr = make_case e1 pat e2 (Pe_EIdentifier "fail_match") in
          return case_expr
        | _ ->
          let* fresh_name = fresh >>| get_id in
          let case_expr =
            make_case (Pe_EIdentifier fresh_name) pat e2 (Pe_EIdentifier "fail_match")
          in
          return @@ Pe_ELet (NoRec, fresh_name, e1, case_expr)))
  | ELetIn (Rec, pat, e1, e2) ->
    let* e1 = pe_expr e1 in
    let* e2 = pe_expr e2 in
    (match pat with
     | PIdentifier name -> return @@ Pe_ELet (Rec, name, e1, e2)
     | PUnit -> return @@ Pe_ELet (Rec, "()", e1, e2)
     | _ ->
       let* fresh_name = fresh >>| get_id in
       let case_expr =
         make_case (Pe_EIdentifier fresh_name) pat e2 (Pe_EIdentifier "fail_match")
       in
       return @@ Pe_ELet (Rec, fresh_name, e1, case_expr))

and pe_match to_match = function
  | (p, e) :: tl ->
    let checks = check_pattern to_match p in
    let decls = check_declaration to_match p in
    let* e = pe_expr e in
    let let_in =
      match decls with
      | Pe_Nonrec decl_list ->
        List.fold_right decl_list ~init:e ~f:(fun (name, value) acc ->
          Pe_ELet (NoRec, name, value, acc))
      | Pe_Rec decl_list ->
        List.fold_right decl_list ~init:e ~f:(fun (name, value) acc ->
          Pe_ELet (Rec, name, value, acc))
    in
    if List.is_empty checks
    then return let_in
    else
      let* match_e = pe_match to_match tl in
      return @@ make_condition checks let_in match_e
  | _ -> return @@ Pe_EIdentifier "fail_match"
;;

let pe_program = function
  | NoRecDecl decl_list ->
    let* decls =
      map decl_list ~f:(fun (Ast.DDeclaration (pat, e)) ->
        let* e = pe_expr e in
        match pat with
        | PIdentifier name -> return (name, e)
        | PUnit -> return ("()", e)
        | _ ->
          let* fresh_name = fresh >>| get_id in
          return (fresh_name, e))
    in
    return (Pe_Nonrec decls)
  | RecDecl decl_list ->
    let* decls =
      map decl_list ~f:(fun (Ast.DDeclaration (pat, e)) ->
        let* e = pe_expr e in
        match pat with
        | PIdentifier v -> return (v, e)
        | _ -> return ("()", e))
      (* TODO: более информативное сообщение *)
    in
    return (Pe_Rec decls)
;;

let pe_structure program =
  let rec helper = function
    | [] -> return []
    | hd :: tl ->
      let* hd = pe_program hd in
      let* tl = helper tl in
      return @@ (hd :: tl)
  in
  helper program
;;

let rec get_binds_expr = function
  | EConstraint (e, _) -> get_binds_expr e
  | EConst _ | EUnit | ENill -> StrSet.empty
  | EIdentifier ident -> StrSet.singleton ident
  | ECons (e1, e2) -> StrSet.union (get_binds_expr e1) (get_binds_expr e2)
  | EApplication (e1, e2) -> StrSet.union (get_binds_expr e1) (get_binds_expr e2)
  | EFun (pat, e) -> StrSet.union (get_binds_pat pat) (get_binds_expr e)
  | EIf (e1, e2, e3) ->
    StrSet.union_list [ get_binds_expr e1; get_binds_expr e2; get_binds_expr e3 ]
  | ELetIn (_, p, e1, e2) ->
    StrSet.union (get_binds_pat p) (StrSet.union (get_binds_expr e1) (get_binds_expr e2))
  | EMatch (e, p_list) ->
    StrSet.union_list
      (get_binds_expr e
       :: List.map p_list ~f:(fun (p, e) ->
         StrSet.union (get_binds_pat p) (get_binds_expr e)))
  | ETuple e_list -> StrSet.union_list @@ List.map e_list ~f:get_binds_expr

and get_binds_declaration = function
  | NoRecDecl decl_list ->
    List.fold decl_list ~init:StrSet.empty ~f:(fun acc (DDeclaration (pat, e)) ->
      StrSet.union acc (StrSet.union (get_binds_pat pat) (get_binds_expr e)))
  | RecDecl decl_list ->
    List.fold decl_list ~init:StrSet.empty ~f:(fun acc (DDeclaration (pat, e)) ->
      StrSet.union acc (StrSet.union (get_binds_pat pat) (get_binds_expr e)))
;;

let create_bundle structure =
  let make_id id =
    let is_digit = function
      | '0' .. '9' -> true
      | _ -> false
    in
    let char_to_digit c = Char.to_int c - Char.to_int '0' in
    let rec helper acc = function
      | [] -> Some acc
      | hd :: tl ->
        if is_digit hd then helper ((acc * 10) + char_to_digit hd) tl else None
    in
    let char_list = String.to_list id in
    match List.length char_list with
    | x when x >= 2 && x <= 10 && Char.equal (List.hd_exn char_list) 'a' ->
      helper 0 (List.tl_exn char_list)
    | _ -> None
  in
  let idents =
    List.fold_left structure ~init:StrSet.empty ~f:(fun acc t ->
      StrSet.union acc (get_binds_declaration t))
  in
  Set.filter_map (module Int) idents ~f:make_id
;;

let run_pe structure = run (pe_structure structure) (create_bundle structure) 0

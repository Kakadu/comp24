(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Pe_ast


let const_to_str = function
  | Pe_CBool b -> if b then "true" else "false"
  | Pe_Cint i -> Format.sprintf "%i" i
;;

let rec expr_to_str = function
  | Pe_EUnit -> "()"
  | Pe_ENill -> "[]"
  | Pe_EIdentifier a -> a
  | Pe_EConst c -> const_to_str c
  | Pe_EVar id -> id
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
           (fun acc (name, e) -> acc ^ Format.sprintf "\nlet %s = %s" name (expr_to_str e))
           ""
           tl)
  | Pe_Rec decl_list ->
    (match decl_list with
     | [] -> ""
     | (name, e) :: tl ->
       Format.sprintf "let rec %s = %s" name (expr_to_str e)
       ^ List.fold_left
           (fun acc (name, e) -> acc ^ Format.sprintf "\nand %s = %s" name (expr_to_str e))
           ""
           tl)
;;

let pp_pe_expr ppf expr = Format.fprintf ppf "%s" (expr_to_str expr)

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
  | Tuple i -> Pe_EApp (Pe_EApp (Pe_EVar "tuple_element", e), Pe_EConst (Pe_Cint i))
  | Cons_head -> Pe_EApp (Pe_EVar "list_head", e)
  | Cons_tail -> Pe_EApp (Pe_EVar "list_tail", e)
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

let make_apply op expr1 expr2 = Pe_EApp (Pe_EApp (Pe_EIdentifier op, expr1), expr2)

module StrSet = struct
  open Base

  type t = (string, String.comparator_witness) Set.t

  let empty = Set.empty (module String)
  let singleton str = Set.singleton (module String) str
  let union = Set.union
  let union_list lst = Set.union_list (module String) lst
  let find s str = Set.mem s str
  let add = Set.add
  let to_list = Set.to_list
  let of_list = Set.of_list (module String)
  let fold = Set.fold
  let diff = Set.diff
end



type bindings = (int, Int.comparator_witness) Set.t

let contains ng id =
  match Set.find ng ~f:(Int.equal id) with
  | Some _ -> true
  | None -> false
;;

module MonadCounter = struct
  open Base

  type 'a t = bindings * int -> bindings * int * 'a

  let return x (binds, var) = binds, var, x

  let fresh (binds, var) =
    let rec helper num = if contains binds num then helper (num + 1) else num in
    let next = helper var in
    binds, next + 1, next
  ;;

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    fun t ->
    let binds, var, x = m t in
    f x (binds, var)
  ;;

  let ( >>= ) = bind
  let ( let* ) = bind

  let ( >>| ) (m : 'a t) (f : 'a -> 'b) : 'b t =
    fun t ->
    let binds, var, x = m t in
    binds, var, f x
  ;;

  let run (m : 'a t) binds start = m (binds, start)

  let map (xs : 'a list) ~(f : 'a -> 'b t) : 'b list t =
    let* xs =
      List.fold xs ~init:(return []) ~f:(fun acc x ->
        let* acc = acc in
        let* x = f x in
        return (x :: acc))
    in
    return @@ List.rev xs
  ;;

  let fold_left (xs : 'a list) ~(init : 'b t) ~(f : 'b -> 'a -> 'b t) : 'b t =
    List.fold xs ~init ~f:(fun acc x ->
      let* acc = acc in
      f acc x)
  ;;

  let fold_right xs ~init ~f =
    List.fold_right xs ~init ~f:(fun x acc ->
      let* acc = acc in
      f x acc)
  ;;
end

let rec get_binds_pat =
  function
  | PConstraint (pat, _) -> get_binds_pat pat
  | PAny | PConst _ | PNill | PUnit  -> StrSet.empty
  | PIdentifier ident -> StrSet.singleton ident
  | PCons (p1, p2) -> StrSet.union (get_binds_pat p1) (get_binds_pat p2)
  | PTuple pl ->
    Base.List.fold pl ~init:StrSet.empty ~f:(fun acc p ->
      StrSet.union acc (get_binds_pat p))
;;

let check_pat expr pat =
  let rec helper add expr = function
    | PConstraint (p, _) -> helper add expr p
    | PConst c ->
      (match c with
       |  _ -> [ make_apply "( = )" expr (const_to_peconst c) ])
    | PTuple pl ->
      let t = List.mapi pl ~f:(fun i p -> helper true (get_element expr (Tuple i)) p) in
      List.concat t
    | PCons (l, r) ->
      let rec length l = function
        | Ast.PCons (_, r) -> length (l + 1) r
        | _ -> l
      in
      let min_length = length 0 r in
      let list_length = Pe_EApp (Pe_EIdentifier "list_len", expr) in
      let check = make_apply "( > )" list_length (Pe_EConst (Pe_Cint min_length)) in
      let l = helper true (get_element expr Cons_head) l in
      let r = helper false (get_element expr Cons_tail) r in
      if add then (check :: l) @ r else l @ r
    | _ -> []
  in
  helper true expr pat
;;

let check_decls expr pat =
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
  let decls = List.map (StrSet.to_list names) ~f:(fun name -> (name, create_expr name)) in
  Pe_Nonrec decls
;;



let make_condition checks e1 e2 =
  let cond =
    List.fold (List.tl_exn checks) ~init:(List.hd_exn checks) ~f:(fun acc a ->
      make_apply "( && )" acc a)
  in
  Pe_EIf (cond, e1, e2)
;;

let make_case expr pat case_expr not_match_expr =
  let checks = check_pat expr pat in
  let decl = check_decls expr pat in
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

open MonadCounter
let get_id i = "a" ^ Int.to_string i

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
       let case_expr = make_case (Pe_EIdentifier fresh_name) pat new_body (Pe_EIdentifier "fail_match") in
       return @@ Pe_EFun (new_args, Pe_ELet (NoRec, fresh_name, to_match, case_expr)))
  | EMatch (e_last, case_list) ->
    let* e = pe_expr e_last in
    (match e_last with
     | EIdentifier _ | EConst _ -> pe_match e case_list
     | _ ->
       let* fresh_name = fresh >>| get_id in
       let* e_match = pe_match (Pe_EIdentifier fresh_name) case_list in
       return @@ (Pe_ELet (NoRec, fresh_name, e, e_match)))
  | ELetIn (NoRec, pat, e1, e2) ->
    let* e1 = pe_expr e1 in
    let* e2 = pe_expr e2 in
    (match pat with
     | PIdentifier name -> return @@ Pe_ELet (NoRec, name, e1, e2)
     | _ ->
       (match e1 with
        | Pe_EIdentifier _ ->
          let case_expr = make_case e1 pat e2 (Pe_EIdentifier "fail_match") in
          return case_expr
        | _ ->
          let* fresh_name = fresh >>| get_id in
          let case_expr = make_case (Pe_EIdentifier fresh_name) pat e2 (Pe_EIdentifier "fail_match") in
          return @@ Pe_ELet (NoRec, fresh_name, e1, case_expr)))
    | ELetIn (Rec, pat, e1, e2) ->
      let* decl = pe_case [ pat, e1 ] in
      let* e = pe_expr e2 in
      let result =
        match decl with
        | Pe_Nonrec decl_list ->
          List.fold_right decl_list ~init:e ~f:(fun (name, value) acc -> Pe_ELet (NoRec, name, value, acc))
        | Pe_Rec decl_list ->
          List.fold_right decl_list ~init:e ~f:(fun (name, value) acc -> Pe_ELet (Rec, name, value, acc))
      in
      return result

and pe_match to_match = function
  | (p, e) :: tl ->
    let checks = check_pat to_match p in
    let decls = check_decls to_match p in
    let* e = pe_expr e in
    let let_in =
      (match decls with
       | Pe_Nonrec decl_list ->
         List.fold_right decl_list ~init:e ~f:(fun (name, value) acc -> Pe_ELet (NoRec, name, value, acc))
       | Pe_Rec decl_list ->
         List.fold_right decl_list ~init:e ~f:(fun (name, value) acc -> Pe_ELet (Rec, name, value, acc)))
    in
    if List.is_empty checks
    then return let_in
    else
      let* match_e = pe_match to_match tl in
      return @@ make_condition checks let_in match_e
  | _ -> return @@ Pe_EIdentifier "fail_match"

and pe_case decl_list =
  let f1 (pat, e) =
    let* e = pe_expr e in
    return
      (match pat with
       | PIdentifier v -> v, e
       | _ -> "", e)
  in
  let* new_decls = map decl_list ~f:f1 in
  return @@ Pe_Rec new_decls
;;

let pe_declaration = function
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
        | _ -> return ("()", e)) (* TODO: более информативное сообщение *)
    in
    return (Pe_Rec decls)
;;

let pe_structure program =
  let rec helper = function
    | [] -> return []
    | hd :: tl ->
      let* hd = pe_declaration hd in
      let* tl = helper tl in
      return @@ hd :: tl
  in
  helper program
;;

let rec get_binds_expr = function
  | EConstraint (e, _) -> get_binds_expr e
  | EConst _ | EUnit |ENill-> StrSet.empty
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

and get_binds_case (pat, e) = StrSet.union (get_binds_pat pat) (get_binds_expr e)

and get_binds_declaration = function
  | NoRecDecl decl_list ->
    List.fold decl_list ~init:StrSet.empty ~f:(fun acc (DDeclaration (pat, e)) ->
      StrSet.union acc (StrSet.union (get_binds_pat pat) (get_binds_expr e)))
  | RecDecl decl_list ->
     List.fold decl_list ~init:StrSet.empty ~f:(fun acc (DDeclaration (pat, e)) ->
      StrSet.union acc (StrSet.union (get_binds_pat pat) (get_binds_expr e)))
;;

let make_binds structure =
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

let run_pe structure = run (pe_structure structure) (make_binds structure) 0
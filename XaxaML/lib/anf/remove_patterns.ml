(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(* RP is short for Remove Patterns *)

type rp_const =
  | Rp_c_int of int
  | Rp_c_bool of bool
  | Rp_c_empty_list
  | Rp_c_unit

type rp_expr =
  | Rp_e_const of rp_const
  | Rp_e_ident of string
  | Rp_e_ite of rp_expr * rp_expr * rp_expr
  | Rp_e_fun of string list * rp_expr
  | Rp_e_app of rp_expr * rp_expr
  | Rp_e_let of rp_decl * rp_expr
  | Rp_e_cons_list of rp_expr * rp_expr
  | Rp_e_tuple of rp_expr list

and rp_decl =
  | Rp_non_rec of string * rp_expr
  | Rp_rec of (string * rp_expr) list

type rp_program = rp_decl list

let const_to_str = function
  | Rp_c_bool b -> if b then "true" else "false"
  | Rp_c_int i -> Format.sprintf "%i" i
  | Rp_c_empty_list -> "[]"
  | Rp_c_unit -> "()"
;;

module PP : sig
  val pp_rp_program : Format.formatter -> rp_program -> unit
  val pp_rp_expr : Format.formatter -> rp_expr -> unit
end = struct
  let rec expr_to_str = function
    | Rp_e_const c -> const_to_str c
    | Rp_e_ident id -> id
    | Rp_e_ite (e1, e2, e3) ->
      Format.sprintf
        "if %s\nthen %s\nelse %s"
        (expr_to_str e1)
        (expr_to_str e2)
        (expr_to_str e3)
    | Rp_e_fun (args, e) ->
      Format.sprintf
        "(fun%s -> %s)"
        (List.fold_left (fun acc name -> acc ^ " " ^ name) "" args)
        (expr_to_str e)
    | Rp_e_app (e1, e2) -> Format.sprintf "(%s %s)" (expr_to_str e1) (expr_to_str e2)
    | Rp_e_let (Rp_non_rec (name, e1), e2) ->
      Format.sprintf "let %s = %s in\n%s" name (expr_to_str e1) (expr_to_str e2)
    | Rp_e_let (Rp_rec decl_list, e2) ->
      let name1, e1 = List.hd decl_list in
      let tl = List.tl decl_list in
      Format.sprintf "let rec %s = %s" name1 (expr_to_str e1)
      ^ List.fold_left
          (fun acc (name, e) -> acc ^ Format.sprintf " and %s = %s" name (expr_to_str e))
          ""
          tl
      ^ Format.sprintf " in\n%s" (expr_to_str e2)
    | Rp_e_cons_list (e1, e2) ->
      Format.sprintf "(%s::%s)" (expr_to_str e1) (expr_to_str e2)
    | Rp_e_tuple e_list ->
      Format.sprintf
        "(%s)"
        (expr_to_str (List.hd e_list)
         ^ List.fold_left
             (fun acc e -> acc ^ Format.sprintf ", %s" (expr_to_str e))
             ""
             (List.tl e_list))
  ;;

  let toplevel_to_str = function
    | Rp_non_rec (name, e) -> Format.sprintf "let %s = %s" name (expr_to_str e)
    | Rp_rec decl_list ->
      let name1, e1 = List.hd decl_list in
      let tl = List.tl decl_list in
      Format.sprintf "let rec %s = %s" name1 (expr_to_str e1)
      ^ List.fold_left
          (fun acc (name, e) -> acc ^ Format.sprintf "\nand %s = %s" name (expr_to_str e))
          ""
          tl
  ;;

  let pp_rp_expr ppf expr = Format.fprintf ppf "%s" (expr_to_str expr)

  let pp_rp_program ppf p =
    let len = List.length p in
    List.iteri
      (fun i a ->
        if i = len - 1
        then Format.fprintf ppf "%s" (toplevel_to_str a)
        else Format.fprintf ppf "%s\n\n" (toplevel_to_str a))
      p
  ;;
end

module ToAst : sig
  val convert_program : rp_program -> Ast.program
end = struct
  open Ast
  open Base

  let const_to_ast = function
    | Rp_c_bool b -> C_bool b
    | Rp_c_int i -> C_int i
    | Rp_c_unit -> C_unit
    | Rp_c_empty_list -> C_empty_list
  ;;

  let to_pattern name = P_val name

  let rec expr_to_ast = function
    | Rp_e_const c -> E_const (const_to_ast c)
    | Rp_e_app (l, r) -> E_app (expr_to_ast l, expr_to_ast r)
    | Rp_e_cons_list (l, r) -> E_cons_list (expr_to_ast l, expr_to_ast r)
    | Rp_e_fun (args, body) ->
      let pats = List.map args ~f:to_pattern in
      E_fun (List.hd_exn pats, List.tl_exn pats, expr_to_ast body)
    | Rp_e_ident v -> E_ident v
    | Rp_e_ite (e1, e2, e3) -> E_ite (expr_to_ast e1, expr_to_ast e2, expr_to_ast e3)
    | Rp_e_let (decl, e) -> E_let (decl_to_ast decl, expr_to_ast e)
    | Rp_e_tuple e_list ->
      let e_list = List.map e_list ~f:expr_to_ast in
      E_tuple (List.hd_exn e_list, List.tl_exn e_list)

  and decl_body_to_ast (name, e) = to_pattern name, None, expr_to_ast e

  and decl_to_ast = function
    | Rp_non_rec (name, e) -> Non_rec (decl_body_to_ast (name, e))
    | Rp_rec decls -> Rec (List.map decls ~f:decl_body_to_ast)
  ;;

  let convert_program p = List.map p ~f:(fun x -> Let_decl (decl_to_ast x))
end

open Base
open Common

let rec get_idents = function
  | Ast.P_typed (pat, _) -> get_idents pat
  | P_any | P_const _ -> StrSet.empty
  | P_val ident -> StrSet.singleton ident
  | P_cons_list (p1, p2) -> StrSet.union (get_idents p1) (get_idents p2)
  | P_tuple (hd, tl) -> StrSet.union (get_idents hd) (get_idents_from_list tl)

and get_idents_from_list pat_list =
  List.fold pat_list ~init:StrSet.empty ~f:(fun acc p -> StrSet.union acc (get_idents p))
;;

let convert_const = function
  | Ast.C_bool b -> Rp_c_bool b
  | Ast.C_int i -> Rp_c_int i
  | Ast.C_empty_list -> Rp_c_empty_list
  | Ast.C_unit -> Rp_c_unit
;;

type unpack =
  | Tuple of int
  | List_hd
  | List_tl
  | Value

let unpack_expr e = function
  | Tuple i -> Rp_e_app (Rp_e_app (Rp_e_ident "#unpack_tuple", e), Rp_e_const (Rp_c_int i))
  | List_hd -> Rp_e_app (Rp_e_ident "#list_hd", e)
  | List_tl -> Rp_e_app (Rp_e_ident "#list_tl", e)
  | Value -> e
;;

let unpack_pat_checks expr pat =
  let rec get_min_lenght l = function
    | Ast.P_cons_list (_, r) -> get_min_lenght (l + 1) r
    | _ -> l
  in
  let rec helper add_list cur = function
    | Ast.P_typed (p, _) -> helper add_list cur p
    | P_const c ->
      (match c with
       | C_unit -> []
       | _ -> [ Rp_e_app (Rp_e_app (Rp_e_ident "=", cur), Rp_e_const (convert_const c)) ])
    | P_tuple (a, b) ->
      let t =
        List.mapi (a :: b) ~f:(fun i p -> helper true (unpack_expr cur (Tuple i)) p)
      in
      List.concat t
    | P_cons_list (l, r) ->
      let min_length = get_min_lenght 0 r in
      let list_length = Rp_e_app (Rp_e_ident "#list_length", cur) in
      let check =
        Rp_e_app (Rp_e_app (Rp_e_ident ">", list_length), Rp_e_const (Rp_c_int min_length))
      in
      let l = helper true (unpack_expr cur List_hd) l in
      let r = helper false (unpack_expr cur List_tl) r in
      if add_list then (check :: l) @ r else l @ r
    | _ -> []
  in
  helper true expr pat
;;

let unpack_pat_decls expr pat =
  let rec helper name = function
    | Ast.P_typed (p, _) -> helper name p
    | P_cons_list (l, r) ->
      (match helper name l with
       | _ :: _ as lst -> List_hd :: lst
       | _ -> List_tl :: helper name r)
    | P_tuple (a, b) ->
      let t = List.map (a :: b) ~f:(helper name) in
      (match List.findi t ~f:(fun _ a -> not @@ List.is_empty a) with
       | Some (i, lst) -> Tuple i :: lst
       | None -> [])
    | P_val v when String.equal v name -> [ Value ]
    | _ -> []
  in
  let create_expr name =
    List.fold_left (helper name pat) ~init:expr ~f:(fun acc unpack ->
      unpack_expr acc unpack)
  in
  let names = get_idents pat in
  List.map (StrSet.to_list names) ~f:(fun name -> Rp_non_rec (name, create_expr name))
;;

let create_if checks e1 e2 =
  let cond =
    List.fold (List.tl_exn checks) ~init:(List.hd_exn checks) ~f:(fun acc a ->
      Rp_e_app (Rp_e_app (Rp_e_ident "&&", acc), a))
  in
  Rp_e_ite (cond, e1, e2)
;;

let create_case to_match pat case_expr not_match_expr =
  let checks = unpack_pat_checks to_match pat in
  let decls = unpack_pat_decls to_match pat in
  let let_expr =
    List.fold_right decls ~init:case_expr ~f:(fun decl_body acc ->
      Rp_e_let (decl_body, acc))
  in
  if List.is_empty checks then let_expr else create_if checks let_expr not_match_expr
;;

let match_failure = Rp_e_ident "#match_failure"

let rec rp_expr = function
  | Ast.E_typed (e, _) -> rp_expr e
  | E_const c -> Rp_e_const (convert_const c)
  | E_ident v -> Rp_e_ident v
  | E_app (e1, e2) ->
    let e1 = rp_expr e1 in
    let e2 = rp_expr e2 in
    Rp_e_app (e1, e2)
  | E_ite (e1, e2, e3) ->
    let e1 = rp_expr e1 in
    let e2 = rp_expr e2 in
    let e3 = rp_expr e3 in
    Rp_e_ite (e1, e2, e3)
  | E_cons_list (e1, e2) ->
    let e1 = rp_expr e1 in
    let e2 = rp_expr e2 in
    Rp_e_cons_list (e1, e2)
  | E_tuple (e, e_list) ->
    let e = rp_expr e in
    let e_list = List.map e_list ~f:(fun e -> rp_expr e) in
    Rp_e_tuple (e :: e_list)
  | E_fun (first, other, body) ->
    let last_args = first :: other in
    let new_args =
      List.mapi last_args ~f:(fun i arg ->
        match arg with
        | P_val v -> v
        | _ -> "#" ^ Int.to_string i)
    in
    let args_to_match =
      List.filter_mapi last_args ~f:(fun i arg ->
        match arg with
        | P_val _ -> None
        | _ -> Some ("#" ^ Int.to_string i))
    in
    let pat_list =
      List.filter last_args ~f:(function
        | P_val _ -> false
        | _ -> true)
    in
    let new_body = rp_expr body in
    (match List.length args_to_match with
     | 0 -> Rp_e_fun (new_args, new_body)
     | 1 ->
       let pat = List.hd_exn pat_list in
       let to_match = Rp_e_ident (List.hd_exn args_to_match) in
       let case_expr = create_case to_match pat new_body match_failure in
       Rp_e_fun (new_args, case_expr)
     | _ ->
       let pat = Ast.P_tuple (List.hd_exn pat_list, List.tl_exn pat_list) in
       let to_match =
         let vals = List.map args_to_match ~f:(fun a -> Rp_e_ident a) in
         Rp_e_tuple vals
       in
       let case_expr = create_case (Rp_e_ident "#t") pat new_body match_failure in
       Rp_e_fun (new_args, Rp_e_let (Rp_non_rec ("#t", to_match), case_expr)))
  | E_match (e, case_list) ->
    (match e with
     | E_ident _ | E_const _ -> rp_match (rp_expr e) case_list
     | _ -> Rp_e_let (Rp_non_rec ("#t", rp_expr e), rp_match (Rp_e_ident "#t") case_list))
  | E_let (Non_rec (pat, _, e1), e2) ->
    let e1 = rp_expr e1 in
    let e2 = rp_expr e2 in
    (match pat with
     | P_val name -> Rp_e_let (Rp_non_rec (name, e1), e2)
     | _ ->
       let case_expr = create_case (Rp_e_ident "#t") pat e2 match_failure in
       Rp_e_let (Rp_non_rec ("#t", e1), case_expr))
  | E_let (Rec decl_list, e) ->
    let decl = rp_rec_decl decl_list in
    let e = rp_expr e in
    Rp_e_let (decl, e)

and rp_match to_match = function
  | (p, e) :: tl ->
    let checks = unpack_pat_checks to_match p in
    let decls = unpack_pat_decls to_match p in
    let e = rp_expr e in
    let let_in = List.fold_right decls ~init:e ~f:(fun d acc -> Rp_e_let (d, acc)) in
    if List.is_empty checks
    then let_in
    else create_if checks let_in (rp_match to_match tl)
  | _ -> Rp_e_ident "#match_failure"

and rp_rec_decl decl_list =
  let f1 (pat, _, e) =
    let e = rp_expr e in
    match pat with
    | Ast.P_val v -> v, e
    | _ -> "", e
  in
  let new_decls = List.map decl_list ~f:f1 in
  Rp_rec new_decls
;;

let rp_toplevel = function
  | Ast.Expr e -> [ Rp_non_rec ("#t", rp_expr e) ]
  | Let_decl (Non_rec (pat, _, e)) ->
    let e = rp_expr e in
    (match pat with
     | P_val name -> [ Rp_non_rec (name, e) ]
     | pat ->
       let to_unpack = Rp_e_ident "#t" in
       let checks = unpack_pat_checks to_unpack pat in
       let decls = unpack_pat_decls to_unpack pat in
       if List.is_empty checks
       then Rp_non_rec ("#t", e) :: decls
       else (
         let ite =
           create_if checks (Rp_e_const Rp_c_unit) (Rp_e_ident "#match_failure")
         in
         Rp_non_rec ("#t", e) :: Rp_non_rec ("(#tt)", ite) :: decls))
  | Let_decl (Rec decl_list) -> [ rp_rec_decl decl_list ]
;;

let run_remove_patterns_program program =
  let rec helper = function
    | [] -> []
    | hd :: tl -> rp_toplevel hd @ helper tl
  in
  helper program
;;

let run_remove_patterns_expr = rp_expr

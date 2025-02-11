(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

(* RP is short for Remove Patterns *)

type rp_const =
  | Rp_c_int of int
  | Rp_c_bool of bool
  | Rp_c_empty_list
  | Rp_c_unit

type rp_pattern =
  | Rp_p_any
  | Rp_p_val of string
  | Rp_p_const of rp_const
  | Rp_p_tuple of rp_pattern * rp_pattern list
  | Rp_p_cons_list of rp_pattern * rp_pattern

type rp_expr =
  | Rp_e_const of rp_const
  | Rp_e_ident of string
  | Rp_e_ite of rp_expr * rp_expr * rp_expr
  | Rp_e_fun of string list * rp_expr
  | Rp_e_app of rp_expr * rp_expr
  | Rp_e_let of rp_decl * rp_expr
  | Rp_e_match of rp_expr * (rp_pattern * rp_expr) list
  | Rp_e_cons_list of rp_expr * rp_expr
  | Rp_e_tuple of rp_expr * rp_expr list

and rp_decl =
  | Rp_non_rec of string * rp_expr
  | Rp_rec of (string * rp_expr) list

type rp_toplevel =
  | Rp_let_decl of rp_decl
  | Rp_expr of rp_expr

type rp_program = rp_toplevel list

let const_to_str = function
  | Rp_c_bool b -> if b then "true" else "false"
  | Rp_c_int i -> Format.sprintf "%i" i
  | Rp_c_empty_list -> "[]"
  | Rp_c_unit -> "()"
;;

let rec pat_to_str = function
  | Rp_p_const c -> const_to_str c
  | Rp_p_any -> "_"
  | Rp_p_val v -> v
  | Rp_p_tuple (p_hd, p_tl) ->
    Format.sprintf
      "(%s)"
      (pat_to_str p_hd ^ List.fold_left (fun acc p -> acc ^ ", " ^ pat_to_str p) "" p_tl)
  | Rp_p_cons_list (p1, p2) -> Format.sprintf "(%s::%s)" (pat_to_str p1) (pat_to_str p2)
;;

let rec expr_to_str = function
  | Rp_e_const c -> const_to_str c
  | Rp_e_ident id -> id
  | Rp_e_ite (e1, e2, e3) ->
    Format.sprintf
      "\nif %s\nthen %s\nelse %s"
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
  | Rp_e_match (e, case_list) ->
    Format.sprintf "match %s with " (expr_to_str e)
    ^ List.fold_left
        (fun acc (p, e) ->
          acc ^ Format.sprintf "\n| %s -> %s " (pat_to_str p) (expr_to_str e))
        ""
        case_list
  | Rp_e_cons_list (e1, e2) -> Format.sprintf "(%s::%s)" (expr_to_str e1) (expr_to_str e2)
  | Rp_e_tuple (e, e_list) ->
    Format.sprintf
      "(%s)"
      (expr_to_str e
       ^ List.fold_left
           (fun acc e -> acc ^ Format.sprintf ", %s" (expr_to_str e))
           ""
           e_list)
;;

let toplevel_to_str = function
  | Rp_let_decl (Rp_non_rec (name, e)) ->
    Format.sprintf "let %s = %s" name (expr_to_str e)
  | Rp_let_decl (Rp_rec decl_list) ->
    let name1, e1 = List.hd decl_list in
    let tl = List.tl decl_list in
    Format.sprintf "let rec %s = %s" name1 (expr_to_str e1)
    ^ List.fold_left
        (fun acc (name, e) -> acc ^ Format.sprintf "\nand %s = %s" name (expr_to_str e))
        ""
        tl
  | Rp_expr e -> Format.sprintf "%s;;" (expr_to_str e)
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

let rec convert_pat = function
  | Ast.P_any -> Rp_p_any
  | Ast.P_cons_list (a, b) -> Rp_p_cons_list (convert_pat a, convert_pat b)
  | Ast.P_const c -> Rp_p_const (convert_const c)
  | Ast.P_tuple (a, b) -> Rp_p_tuple (convert_pat a, List.map b ~f:convert_pat)
  | Ast.P_typed (p, _) -> convert_pat p
  | Ast.P_val v -> Rp_p_val v
;;

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
    Rp_e_tuple (e, e_list)
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
        | Ast.P_val _ -> None
        | _ -> Some ("#" ^ Int.to_string i))
    in
    let new_e = rp_expr body in
    if List.length args_to_match = 0
    then Rp_e_fun (new_args, new_e)
    else (
      let to_match =
        let vals = List.map args_to_match ~f:(fun a -> Rp_e_ident a) in
        Rp_e_tuple (List.hd_exn vals, List.tl_exn vals)
      in
      let pat =
        let pat_list =
          List.filter_map last_args ~f:(fun arg ->
            match arg with
            | P_val _ -> None
            | p -> Some (convert_pat p))
        in
        Rp_p_tuple (List.hd_exn pat_list, List.tl_exn pat_list)
      in
      Rp_e_fun (new_args, Rp_e_match (to_match, [ pat, new_e ])))
  | E_match (e, case_list) ->
    let e = rp_expr e in
    let case_list = List.map case_list ~f:(fun (p, e) -> convert_pat p, rp_expr e) in
    Rp_e_match (e, case_list)
  | E_let ((Non_rec (_, _, _) as orig), e) ->
    let decls = rp_decl orig in
    let e = rp_expr e in
    (* TODO: add #t as name here *)
    List.fold_right decls ~init:e ~f:(fun decl_body acc -> Rp_e_let (decl_body, acc))
  | E_let ((Rec _ as orig), e) ->
    let decl = List.nth_exn (rp_decl orig) 0 in
    let e = rp_expr e in
    Rp_e_let (decl, e)

and rp_decl = function
  | Non_rec (pat, _, e) ->
    let new_e = rp_expr e in
    (match pat with
     | P_val name -> [ Rp_non_rec (name, new_e) ]
     | pat ->
       let fst = Rp_non_rec ("#t", new_e) in
       let idents = get_idents pat in
       let f1 name =
         Rp_non_rec
           (name, Rp_e_match (Rp_e_ident "#t", [ convert_pat pat, Rp_e_ident name ]))
       in
       fst :: List.map (StrSet.to_list idents) ~f:f1)
  | Rec decl_list ->
    let f1 (pat, _, e) =
      let e = rp_expr e in
      match pat with
      | Ast.P_val v -> v, e
      | _ -> "", e
    in
    let new_decls = List.map decl_list ~f:f1 in
    [ Rp_rec new_decls ]
;;

let rp_toplevel = function
  | Ast.Expr e -> [ Rp_expr (rp_expr e) ]
  | Let_decl d -> List.map (rp_decl d) ~f:(fun x -> Rp_let_decl x)
;;

let run_remove_patterns_program program =
  let rec helper = function
    | [] -> []
    | hd :: tl -> rp_toplevel hd @ helper tl
  in
  helper program
;;

let run_remove_patterns_expr = rp_expr

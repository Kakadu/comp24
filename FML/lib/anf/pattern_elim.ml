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

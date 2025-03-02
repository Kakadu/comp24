(** Copyright 2024-2025, Nikita Lukonenko and Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type pe_const =
  | PECint of int
  | PECBool of bool
  | PECNil
  | PECUnit
  | PECString of string

type pe_expr =
  | PEEConst of pe_const
  | PEEVar of string
  | PEEIf of pe_expr * pe_expr * pe_expr
  | PEEFun of string list * pe_expr
  | PEEApp of pe_expr * pe_expr
  | PEELet of pe_str_item * pe_expr
  | PEECons of pe_expr * pe_expr
  | PEETuple of pe_expr list

and pe_str_item =
  | PENonrec of string * pe_expr
  | PERec of (string * pe_expr) list

type pe_structure = pe_str_item list

let const_to_str = function
  | PECBool b -> if b then "true" else "false"
  | PECint i -> Format.sprintf "%i" i
  | PECNil -> "[]"
  | PECUnit -> "()"
  | PECString s -> s
;;

let rec expr_to_str = function
  | PEEConst c -> const_to_str c
  | PEEVar id -> id
  | PEEIf (e1, e2, e3) ->
    Format.sprintf
      "if %s\nthen %s\nelse %s"
      (expr_to_str e1)
      (expr_to_str e2)
      (expr_to_str e3)
  | PEEFun (args, e) ->
    Format.sprintf
      "(fun%s -> %s)"
      (List.fold_left (fun acc name -> acc ^ " " ^ name) "" args)
      (expr_to_str e)
  | PEEApp (e1, e2) -> Format.sprintf "(%s %s)" (expr_to_str e1) (expr_to_str e2)
  | PEELet (PENonrec (name, e1), e2) ->
    Format.sprintf "let %s = %s in\n%s" name (expr_to_str e1) (expr_to_str e2)
  | PEELet (PERec decl_list, e2) ->
    let name1, e1 = List.hd decl_list in
    let tl = List.tl decl_list in
    Format.sprintf "let rec %s = %s" name1 (expr_to_str e1)
    ^ List.fold_left
        (fun acc (name, e) -> acc ^ Format.sprintf " and %s = %s" name (expr_to_str e))
        ""
        tl
    ^ Format.sprintf " in\n%s" (expr_to_str e2)
  | PEECons (e1, e2) -> Format.sprintf "(%s::%s)" (expr_to_str e1) (expr_to_str e2)
  | PEETuple e_list ->
    Format.sprintf
      "(%s)"
      (expr_to_str (List.hd e_list)
       ^ List.fold_left
           (fun acc e -> acc ^ Format.sprintf ", %s" (expr_to_str e))
           ""
           (List.tl e_list))
;;

let decl_to_str = function
  | PENonrec (name, e) -> Format.sprintf "let %s = %s" name (expr_to_str e)
  | PERec decl_list ->
    let name1, e1 = List.hd decl_list in
    let tl = List.tl decl_list in
    Format.sprintf "let rec %s = %s" name1 (expr_to_str e1)
    ^ List.fold_left
        (fun acc (name, e) -> acc ^ Format.sprintf "\nand %s = %s" name (expr_to_str e))
        ""
        tl
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
  | Tuple i -> PEEApp (PEEApp (PEEVar "tuple_element", e), PEEConst (PECint i))
  | Cons_head -> PEEApp (PEEVar "list_head", e)
  | Cons_tail -> PEEApp (PEEVar "list_tail", e)
  | Other -> e
;;

let const_to_peconst =
  let open Ast in
  function
  | CString s -> PECString s
  | CInt i -> PECint i
  | CBool b -> PECBool b
  | CNil -> PECNil
  | CUnit -> PECUnit
;;

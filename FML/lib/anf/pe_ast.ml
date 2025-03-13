(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type rec_flag =
  | Rec
  | NoRec

type pe_const =
  | Pe_Cint of int
  | Pe_CBool of bool

type pe_expr =
  | Pe_EUnit
  | Pe_ENill
  | Pe_EIdentifier of string
  | Pe_EConst of pe_const
  | Pe_EIf of pe_expr * pe_expr * pe_expr
  | Pe_EFun of string list * pe_expr
  | Pe_EApp of pe_expr * pe_expr
  | Pe_ELet of rec_flag * string * pe_expr * pe_expr
  | Pe_ECons of pe_expr * pe_expr
  | Pe_ETuple of pe_expr list

type pe_declaration =
  | Pe_Nonrec of (string * pe_expr) list
  | Pe_Rec of (string * pe_expr) list

type pe_program = pe_declaration list

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
    Format.sprintf "(fun %s -> %s)" (String.concat " " args) (expr_to_str e)
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

let pp_pe_program ppf p =
  let len = List.length p in
  List.iteri
    (fun i a ->
      if i = len - 1
      then Format.fprintf ppf "%s" (decl_to_str a)
      else Format.fprintf ppf "%s\n\n" (decl_to_str a))
    p
;;

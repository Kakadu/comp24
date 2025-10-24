(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type rec_flag =
  | Rec
  | NoRec

type me_const =
  | Me_Cint of int
  | Me_CBool of bool

type me_expr =
  | Me_EUnit
  | Me_ENill
  | Me_EIdentifier of string
  | Me_EConst of me_const
  | Me_EIf of me_expr * me_expr * me_expr
  | Me_EFun of string list * me_expr
  | Me_EApp of me_expr * me_expr
  | Me_ELet of rec_flag * string * me_expr * me_expr
  | Me_ECons of me_expr * me_expr
  | Me_ETuple of me_expr list

type me_declaration =
  | Me_Nonrec of (string * me_expr) list
  | Me_Rec of (string * me_expr) list

type me_program = me_declaration list

let const_to_str = function
  | Me_CBool b when b -> "true"
  | Me_CBool _ -> "false"
  | Me_Cint i -> Format.sprintf "%i" i
;;

let rec expr_to_str = function
  | Me_EUnit -> "()"
  | Me_ENill -> "[]"
  | Me_EIdentifier a -> a
  | Me_EConst c -> const_to_str c
  | Me_EIf (e1, e2, e3) ->
    Format.sprintf
      "if %s\nthen %s\nelse %s"
      (expr_to_str e1)
      (expr_to_str e2)
      (expr_to_str e3)
  | Me_EFun (args, e) ->
    Format.sprintf "(fun %s -> %s)" (String.concat " " args) (expr_to_str e)
  | Me_EApp (e1, e2) -> Format.sprintf "(%s %s)" (expr_to_str e1) (expr_to_str e2)
  | Me_ELet (NoRec, name, e1, e2) ->
    Format.sprintf "let %s = %s in\n%s" name (expr_to_str e1) (expr_to_str e2)
  | Me_ELet (Rec, name1, e1, e2) ->
    Format.sprintf "let rec %s = %s in\n%s" name1 (expr_to_str e1) (expr_to_str e2)
  | Me_ECons (e1, e2) -> Format.sprintf "(%s::%s)" (expr_to_str e1) (expr_to_str e2)
  | Me_ETuple e_list ->
    Format.sprintf
      "(%s)"
      (expr_to_str (List.hd e_list)
       ^ List.fold_left
           (fun acc e -> acc ^ Format.sprintf ", %s" (expr_to_str e))
           ""
           (List.tl e_list))
;;

let decl_to_str = function
  | Me_Nonrec decl_list ->
    (match decl_list with
     | [] -> ""
     | (name, e) :: tl ->
       Format.sprintf "let %s = %s" name (expr_to_str e)
       ^ List.fold_left
           (fun acc (name, e) ->
             acc ^ Format.sprintf "\nlet %s = %s" name (expr_to_str e))
           ""
           tl)
  | Me_Rec decl_list ->
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

let pp_me_program ppf p =
  let len = List.length p in
  List.iteri
    (fun i a ->
      if i = len - 1
      then Format.fprintf ppf "%s" (decl_to_str a)
      else Format.fprintf ppf "%s\n\n" (decl_to_str a))
    p
;;

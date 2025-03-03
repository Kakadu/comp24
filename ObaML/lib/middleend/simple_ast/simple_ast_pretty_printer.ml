(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Format

let binop_vars =
  [ "( * )"
  ; "( / )"
  ; "( + )"
  ; "( - )"
  ; "( = )"
  ; "( == )"
  ; "( <> )"
  ; "( != )"
  ; "( < )"
  ; "( <= )"
  ; "( > )"
  ; "( >= )"
  ; "( && )"
  ; "( || )"
  ]
;;

let print_id fmt = function
  | Ast.Id ident when List.mem ident binop_vars = true ->
    fprintf fmt "%s" (String.sub ident 1 (String.length ident - 2))
  | Ast.Id ident -> fprintf fmt "%s" ident
;;

let print_sid fmt = function
  | Simple_ast.SSpecial SUnit -> fprintf fmt "()"
  | Simple_ast.SId id -> fprintf fmt "%a" print_id id
;;

let rec print_sid_lst fmt = function
  | [] -> ()
  | [ h ] -> fprintf fmt "%a" print_sid h
  | h :: tl -> fprintf fmt "%a %a" print_sid h print_sid_lst tl
;;

let print_const fmt = function
  | Ast.CInt num -> fprintf fmt "%d" num
  | Ast.CString str -> fprintf fmt "%S" str
  | Ast.CBool bool -> fprintf fmt "%s" (string_of_bool bool)
  | Ast.CEmptyList -> fprintf fmt "[]"
  | Ast.CUnit -> fprintf fmt "()"
;;

let rec print_etuple fmt = function
  | [] -> ()
  | [ h ] -> fprintf fmt "%a" print_expr h
  | h :: tl -> fprintf fmt "%a, %a" print_expr h print_etuple tl

and print_expr fmt = function
  | Simple_ast.SEConst const -> fprintf fmt "%a" print_const const
  | Simple_ast.SEVar id -> fprintf fmt "%a" print_id id
  | Simple_ast.SETuple exp_lst -> fprintf fmt "(%a)" print_etuple exp_lst
  | Simple_ast.SEFun (pat_lst, expr) ->
    fprintf fmt "(fun %a -> %a)" print_sid_lst pat_lst print_expr expr
  | Simple_ast.SELet (Ast.Nonrecursive, value_binding, expr) ->
    fprintf fmt "\n\tlet %a in %a" print_value_binding value_binding print_expr expr
  | Simple_ast.SELet (Ast.Recursive, value_binding, expr) ->
    fprintf fmt "\n\tlet rec %a in %a" print_value_binding value_binding print_expr expr
  | Simple_ast.SEApp
      (Simple_ast.SEApp ((Simple_ast.SEVar (Id var_name) as expr1), expr2), expr3)
    when List.mem var_name binop_vars = true ->
    fprintf fmt "(%a %a %a)" print_expr expr2 print_expr expr1 print_expr expr3
  | Simple_ast.SEApp (expr1, expr2) ->
    fprintf fmt "(%a %a)" print_expr expr1 print_expr expr2
  | Simple_ast.SEIf (expr1, expr2, expr3) ->
    fprintf
      fmt
      "\n\tif %a\n\tthen %a\n\telse %a"
      print_expr
      expr1
      print_expr
      expr2
      print_expr
      expr3
  | Simple_ast.SECons (expr1, expr2) ->
    fprintf fmt "(%a :: %a)" print_expr expr1 print_expr expr2

and print_value_binding fmt value_binding =
  let ident, expr = value_binding in
  match expr with
  | Simple_ast.SEFun (ident_lst, fexpr) ->
    fprintf fmt "%a = %a" print_sid_lst (ident :: ident_lst) print_expr fexpr
  | _ -> fprintf fmt "%a = %a" print_sid ident print_expr expr
;;

let rec print_value_binding_lst fmt (value_binding_lst : Simple_ast.svalue_binding list) =
  match value_binding_lst with
  | [] -> ()
  | [ h ] -> fprintf fmt "%a" print_value_binding h
  | h :: tl -> fprintf fmt "%a\nand %a" print_value_binding h print_value_binding_lst tl
;;

let print_structure fmt structure =
  List.iter
    (fun structure_item ->
      match structure_item with
      | Simple_ast.SSILet (Ast.Nonrecursive, value_binding_lst) ->
        fprintf fmt "let %a;;\n\n" print_value_binding_lst value_binding_lst
      | Simple_ast.SSILet (Ast.Recursive, value_binding_lst) ->
        fprintf fmt "let rec %a;;\n\n" print_value_binding_lst value_binding_lst
      | Simple_ast.SSIExpr expr -> fprintf fmt "%a" print_expr expr)
    structure
;;

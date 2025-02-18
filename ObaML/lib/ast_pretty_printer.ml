(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Format

let print_id fmt = function
  | Ast.Id ident -> fprintf fmt "%s" ident
;;

let print_const fmt = function
  | Ast.CInt num -> fprintf fmt "%d" num
  | Ast.CString str -> fprintf fmt "%s" str
  | Ast.CBool bool -> fprintf fmt "%s" (string_of_bool bool)
  | Ast.CEmptyList -> fprintf fmt "[]"
  | Ast.CUnit -> fprintf fmt "()"
;;

let rec print_tuple_type fmt = function
  | [] -> ()
  | [ h ] -> fprintf fmt "%a" print_type h
  | h :: tl -> fprintf fmt "%a * %a" print_type h print_tuple_type tl

and print_type fmt = function
  | Ast.TInt -> fprintf fmt "int"
  | Ast.TString -> fprintf fmt "string"
  | Ast.TBool -> fprintf fmt "bool"
  | Ast.TUnit -> fprintf fmt "unit"
  | Ast.TVar id -> fprintf fmt "%a" print_id id
  | Ast.TTuple typ_lst -> fprintf fmt "(%a)" print_tuple_type typ_lst
  | Ast.TArrow (typ1, typ2) -> fprintf fmt "(%a -> %a)" print_type typ1 print_type typ2
  | Ast.TList typ -> fprintf fmt "(%a) list" print_type typ
;;

let rec print_ptuple fmt = function
  | [] -> ()
  | [ h ] -> fprintf fmt "%a" print_pattern h
  | h :: tl -> fprintf fmt "%a, %a" print_pattern h print_ptuple tl

and print_pattern fmt = function
  | Ast.PAny -> fprintf fmt "_"
  | Ast.PConst const -> fprintf fmt "%a" print_const const
  | Ast.PVar ident -> fprintf fmt "%a" print_id ident
  | Ast.PTuple pat_lst -> fprintf fmt "(%a)" print_ptuple pat_lst
  | Ast.PCons (pat1, pat2) -> fprintf fmt "%a :: %a" print_pattern pat1 print_pattern pat2
  | Ast.PType (pat, typ) -> fprintf fmt "(%a : %a)" print_pattern pat print_type typ
;;

let rec print_pattern_lst fmt = function
  | [] -> ()
  | [ h ] -> fprintf fmt "%a" print_pattern h
  | h :: tl -> fprintf fmt "%a %a" print_pattern h print_pattern_lst tl
;;

let rec print_etuple fmt = function
  | [] -> ()
  | [ h ] -> fprintf fmt "%a" print_expr h
  | h :: tl -> fprintf fmt "%a, %a" print_expr h print_etuple tl

and print_cases fmt = function
  | [] -> ()
  | [ (pat, expr) ] -> fprintf fmt "| %a -> %a" print_pattern pat print_expr expr
  | (pat, expr) :: tl ->
    fprintf fmt "| %a -> %a %a" print_pattern pat print_expr expr print_cases tl

and print_expr fmt = function
  | Ast.EConst const -> fprintf fmt "%a" print_const const
  | Ast.EVar id -> fprintf fmt "%a" print_id id
  | Ast.ETuple exp_lst -> fprintf fmt "(%a)" print_etuple exp_lst
  | Ast.EFun (pat_lst, expr) ->
    fprintf fmt "(fun %a -> %a)" print_pattern_lst pat_lst print_expr expr
  | Ast.ELet (Ast.Nonrecursive, value_binding, expr) ->
    fprintf fmt "let %a in %a" print_value_binding value_binding print_expr expr
  | Ast.ELet (Ast.Recursive, value_binding, expr) ->
    fprintf fmt "let rec %a in %a" print_value_binding value_binding print_expr expr
  | Ast.EApp (expr1, expr2) -> fprintf fmt "(%a %a)" print_expr expr1 print_expr expr2
  | Ast.EMatch (expr, case_lst) ->
    fprintf fmt "match %a with %a" print_expr expr print_cases case_lst
  | Ast.EIf (expr1, expr2, expr3) ->
    fprintf fmt "if %a then %a else %a" print_expr expr1 print_expr expr2 print_expr expr3
  | Ast.ECons (expr1, expr2) -> fprintf fmt "%a :: %a" print_expr expr1 print_expr expr2
  | Ast.EType (expr, typ) -> fprintf fmt "(%a : %a)" print_expr expr print_type typ

and print_value_binding fmt value_binding =
  let pat, expr = value_binding in
  match expr with
  | Ast.EFun (pat_lst, fexpr) ->
    fprintf fmt "%a = %a" print_pattern_lst (pat :: pat_lst) print_expr fexpr
  | _ -> fprintf fmt "%a = %a" print_pattern pat print_expr expr
;;

let rec print_value_binding_lst fmt (value_binding_lst : Ast.value_binding list) =
  match value_binding_lst with
  | [] -> ()
  | [ h ] -> fprintf fmt "%a" print_value_binding h
  | h :: tl -> fprintf fmt "%a and %a" print_value_binding h print_value_binding_lst tl
;;

let print_structure fmt structure =
  List.iter
    (fun structure_item ->
      match structure_item with
      | Ast.SILet (Ast.Nonrecursive, value_binding_lst) ->
        fprintf fmt "let %a;;\n" print_value_binding_lst value_binding_lst
      | Ast.SILet (Ast.Recursive, value_binding_lst) ->
        fprintf fmt "let rec %a;;\n" print_value_binding_lst value_binding_lst
      | Ast.SIExpr expr -> fprintf fmt "%a" print_expr expr)
    structure
;;

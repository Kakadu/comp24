(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Format
open Base
open Utils

let pp_id = Ast.pp_id

let rec pp_constant fmt = function
  | CInt i when i >= 0 -> fprintf fmt "%d" i
  | CInt i -> fprintf fmt "(%d)" i
  | CBool b -> fprintf fmt "%b" b
  | CUnit -> fprintf fmt "()"
  | CNil -> fprintf fmt "[]"

and pp_type_ann fmt = function
  | TAInt -> fprintf fmt "int"
  | TABool -> fprintf fmt "bool"
  | TAUnit -> fprintf fmt "()"
  | TAFun (e1, e2) ->
    let rec pp_rest fmt = function
      | TAFun (e1, e2) -> fprintf fmt "%a -> %a" pp_type_ann e1 pp_rest e2
      | e -> fprintf fmt "%a)" pp_type_ann e
    in
    fprintf fmt "(%a -> %a" pp_type_ann e1 pp_rest e2
  | TATuple (hd1, hd2, tl) -> 
    let xs = hd1 :: hd2 :: tl in
    pp_list ~sep:" * " fmt pp_type_ann xs
  | TAList x -> fprintf fmt "%a list" pp_type_ann x

and pp_pattern fmt = function
  | PConst c -> fprintf fmt "%a" pp_constant c
  | PWild -> fprintf fmt "_"
  | PIdent id -> fprintf fmt "%s" id
  | PTuple (hd1, hd2, tl) ->
    let xs = hd1 :: hd2 :: tl in
    pp_list ~sep:", " fmt pp_pattern xs
  | PList xs -> pp_list ~op:"[" ~cl:"]" ~sep:"; " fmt pp_pattern xs
  | PCons (l, r) -> fprintf fmt "%a :: %a" pp_pattern l pp_pattern r
  | PAnn (pat, ty) -> fprintf fmt "(%a: %a)" pp_pattern pat pp_type_ann ty

and pp_pattern_list fmt p = pp_list ~op:"" ~cl:"" ~sep:" " fmt pp_pattern p

and pp_expr fmt =
  let parenthesize fmt = function
    | (EIfElse _ | EMatch _ | ELetIn _) as e -> fprintf fmt "(%a)" pp_expr e
    | e -> fprintf fmt "%a" pp_expr e
  in
  function
  | EConst c -> fprintf fmt "%a" pp_constant c
  | EVar v -> fprintf fmt "%s" v
  | EApp (e1, e2) ->
    let rec pp_rest fmt = function
      | EApp (e1, e2) -> fprintf fmt "%a %a" pp_rest e1 parenthesize e2
      | e -> fprintf fmt "(%a" parenthesize e
    in
    fprintf fmt "%a %a)" pp_rest e1 parenthesize e2
  | EIfElse (c, t, e) -> fprintf fmt "if %a then %a else %a" pp_expr c pp_expr t pp_expr e
  | EFun (p, e) ->
    fprintf fmt "(fun ";
    pp_pattern_list fmt p;
    fprintf fmt " -> %a)" pp_expr e
  | ELetIn (d, e) -> fprintf fmt "%a in %a" pp_definition d pp_expr e
  | ETuple (hd1, hd2, tl) -> 
    let xs = hd1 :: hd2 :: tl in
    pp_list ~sep:", " fmt pp_expr xs
  | EMatch (e, pe_list) ->
    fprintf fmt "match %a with\n" pp_expr e;
    pp_print_list
      ~pp_sep:Format.pp_print_newline
      (fun fmt (p, e) -> fprintf fmt "| %a -> %a" pp_pattern p parenthesize e)
      fmt
      pe_list
  | EList xs -> pp_list ~op:"[" ~cl:"]" ~sep:"; " fmt pp_expr xs

and pp_definition fmt = function
  | DLet (NonRec, pat, e) -> fprintf fmt "let %a = %a\n" pp_pattern pat pp_expr e
  | DLet (Rec, pat, e) -> fprintf fmt "let rec %a = %a\n" pp_pattern pat pp_expr e
;;

let pp_program fmt p = pp_list ~op:"" ~cl:"" ~sep:"\n" fmt pp_definition p

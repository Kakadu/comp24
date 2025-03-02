(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Parser.Ast
open Format
open Unparser_utils

let rec unparse_pattern ppf = function
  | Pat_any -> fprintf ppf "_"
  | Pat_var s -> fprintf ppf "%s" s
  | Pat_type (p, t) ->
    fprintf ppf "(%a : %a)" unparse_pattern p Typexpr_unparser.unparse_typexpr t
  | Pat_constant c -> fprintf ppf "%a" Const_unparser.unparse_const c
  | Pat_tuple l -> fprintf ppf "(%a)" unparse_tuple l
  | Pat_or (p1, p2) -> fprintf ppf "(%a | %a)" unparse_pattern p1 unparse_pattern p2
  | Pat_construct (s, None) -> fprintf ppf "%s" s
  | Pat_construct ("::", Some (Pat_tuple [ l; r ])) ->
    fprintf ppf "(%a :: %a)" unparse_pattern l unparse_pattern r
  | Pat_construct (s, Some l) -> fprintf ppf "%s %a" s unparse_pattern l

and unparse_tuple ppf l = list_unparser ppf l ~f:unparse_pattern ~s:", "

and unparse_pattern_list ppf l =
  list_unparser ppf l ~f:unparse_pattern ~s:" " ~add_before:true
;;

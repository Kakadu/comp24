(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Parser.Ast
open Format
open Unparser_utils

let rec unparse_expr ppf = function
  | Exp_ident s -> fprintf ppf "%s" s
  | Exp_constant c -> fprintf ppf "%a" Const_unparser.unparse_const c
  | Exp_type (e, t) ->
    fprintf ppf "(%a : %a)" unparse_expr e Typexpr_unparser.unparse_typexpr t
  | Exp_let (r, l, e) ->
    fprintf
      ppf
      "let%a %a in %a"
      unparse_rec_flag
      r
      unparse_let_binding_list
      l
      unparse_expr
      e
  | Exp_fun (pl, e) ->
    fprintf ppf "fun%a -> %a" Pattern_unparser.unparse_pattern_list pl unparse_expr e
  | Exp_function cl -> fprintf ppf "function %a" unparse_case_list cl
  | Exp_apply (e1, e2) -> fprintf ppf "%a %a" unparse_expr e1 unparse_apply_helper e2
  | Exp_match (e, cl) ->
    fprintf ppf "match %a with %a" unparse_expr e unparse_case_list cl
  | Exp_tuple el -> fprintf ppf "(%a)" unparse_tuple el
  | Exp_construct (s, None) -> fprintf ppf "%s" s
  | Exp_construct ("::", Some e) -> fprintf ppf "(::) %a" unparse_expr e
  | Exp_construct (s, Some e) -> fprintf ppf "%s %a" s unparse_expr e
  | Exp_if (e1, e2, None) -> fprintf ppf "if %a then %a" unparse_expr e1 unparse_expr e2
  | Exp_if (e1, e2, Some e3) ->
    fprintf ppf "if %a then %a else %a" unparse_expr e1 unparse_expr e2 unparse_expr e3
  | Exp_sequence (e1, e2) -> fprintf ppf "%a;\n%a" unparse_expr e1 unparse_expr e2

and unparse_let_binding_list ppf l =
  let unparse_let_binding ppf = function
    | Pat_binding (p, e) ->
      fprintf ppf "%a = %a" Pattern_unparser.unparse_pattern p unparse_expr e
    | Val_binding (s, pl, e) ->
      fprintf ppf "%s%a = %a" s Pattern_unparser.unparse_pattern_list pl unparse_expr e
  in
  list_unparser ppf l ~f:unparse_let_binding ~s:" and "

and unparse_case_list ppf l =
  let unparse_case ppf c =
    fprintf ppf "| %a -> %a" Pattern_unparser.unparse_pattern c.left unparse_expr c.right
  in
  list_unparser ppf l ~f:unparse_case ~s:" "

and unparse_tuple ppf l = list_unparser ppf l ~f:unparse_expr ~s:", "

and unparse_apply_helper ppf e =
  match e with
  | Exp_ident _ | Exp_constant _ -> fprintf ppf "%a" unparse_expr e
  | _ -> fprintf ppf "(%a)" unparse_expr e
;;

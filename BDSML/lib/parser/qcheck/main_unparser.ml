(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Parser.Ast
open Format
open Unparser_utils

let unparse_structure_item ppf = function
  | Str_eval e -> fprintf ppf "%a" Expr_unparser.unparse_expr e
  | Str_value (r, l) ->
    fprintf ppf "let%a %a" unparse_rec_flag r Expr_unparser.unparse_let_binding_list l
;;

let unparse_structure ppf sl =
  list_unparser ppf sl ~f:unparse_structure_item ~s:";;\n";
  fprintf ppf ";;"
;;

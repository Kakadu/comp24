(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Parser.Ast
open Format
open Unparser_utils

let rec unparse_typexpr ppf = function
  | Type_single s -> fprintf ppf "%s" s
  | Type_params (t, id) -> fprintf ppf "%a %s" unparse_typexpr t id
  | Type_tuple l -> fprintf ppf "%a" unparse_tuple l
  | Type_fun l -> fprintf ppf "(%a)" unparse_fun l

and unparse_tuple ppf l =
  List.iteri
    (fun i x ->
      if i <> 0 then fprintf ppf " * " else ();
      fprintf ppf "%a" unparse_typexpr x)
    l

and unparse_fun ppf l = list_unparser ppf l ~f:unparse_typexpr ~s:" -> "

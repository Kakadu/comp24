(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Parser.Ast
open Format

let unparse_const ppf = function
  | Const_int i -> fprintf ppf "%d" i
  | Const_char c -> fprintf ppf "\'%c\'" c
  | Const_string s -> fprintf ppf "\"%s\"" s
  | Const_bool s -> fprintf ppf "%s" @@ if s then "true" else "false"
  | Const_unit -> fprintf ppf "()"
;;

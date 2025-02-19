(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let print_expression_parsing_result ast =
  Format.printf "%a\n" Ast.AbstractSyntaxTree.pp_expression ast
;;

let print_program_parsing_result ast =
  Format.printf "%a\n" Ast.AbstractSyntaxTree.pp_program ast
;;

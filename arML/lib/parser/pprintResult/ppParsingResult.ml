(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let print_parser_result ast = Format.printf "%a\n" Ast.pp_program ast

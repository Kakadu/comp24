(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

val parse_program : ?print_ast:bool -> string -> (Ast.definition list, string) result

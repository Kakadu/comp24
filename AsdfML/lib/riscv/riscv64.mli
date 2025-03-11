(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

val compile : ?out_file:string -> ?print_anf:bool -> Anf_ast.program -> (unit, 'a) result

(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)


val inference_program : Ast.definition list -> (Tast.tdefinition list, Types.error) result

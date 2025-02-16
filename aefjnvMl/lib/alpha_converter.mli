(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type core_type_flag =
  | On
  | Off

val rename_ast_with_uniq
  :  Common.Ast.structure_item list
  -> (Common.Ast.structure_item list, Common.Errors.error) result
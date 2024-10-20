(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

val generalise
  :  Help.SetString.t
  -> Ast.pattern_no_constraint
  -> Ast.typeName
  -> (StatementInfer.state, unit) StatementInfer.t

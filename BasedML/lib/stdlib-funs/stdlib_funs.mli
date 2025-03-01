(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type llvm_name = string
type name = string

type fun_type =
  | UserFun
  | SystemFun

type std_fun = llvm_name * name * fun_type * Ast.type_name

val stdlib_funs : std_fun list

(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

type llvm_name = string
type name = string

type is_system =
  | UserFun
  | SystemFun

type is_vararg =
  | NotVararg
  | Vararg

type fun_type = is_system * is_vararg
type std_fun = llvm_name * name * fun_type * Ast.type_name

val stdlib_funs : std_fun list

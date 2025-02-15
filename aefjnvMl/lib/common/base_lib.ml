(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Typedtree

let int_ty_op = TArrow (TPrim "int", TArrow (TPrim "int", TPrim "int"))
let op_mul = "*"
let sig_op_mul = "*"
let op_div = "/"
let op_plus = "+"
let op_minus = "-"
let comp_ty_op binder = TArrow (TVar binder, TArrow (TVar binder, TPrim "bool"))
let op_less_eq = "<="
let op_more_eq = ">="
let op_less = "<"
let op_more = ">"
let op_eq = "="
let op_2eq = "=="
let op_not_eq = "<>"
let bool_ty_op = TArrow (TPrim "bool", TArrow (TPrim "bool", TPrim "bool"))
let op_and = "&&"
let op_or = "||"
let sig_un_op_minus = TArrow (TPrim "int", TPrim "int")
let un_op_minus = "-"
let sig_un_op_not = TArrow (TPrim "bool", TPrim "bool")
let un_op_not = "!"
let un_op_prefix = "~"
let sig_func_print_int = TArrow (TPrim "int", TPrim "unit")
let func_print_int = "print_int"

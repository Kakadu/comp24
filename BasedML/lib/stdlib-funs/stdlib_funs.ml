(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

type llvm_name = string
type name = string
type std_fun = llvm_name * name * type_name

let bin_op arg1 arg2 ret = TFunction (arg1, TFunction (arg2, ret))

let stdlib_funs : std_fun list =
  [ "( + )", "plus_mlint", bin_op TInt TInt TInt
  ; "( - )", "minus_mlint", bin_op TInt TInt TInt
  ; "( * )", "mult_mlint", bin_op TInt TInt TInt
  ; "( / )", "div_mlint", bin_op TInt TInt TInt
  ; "( < )", "l_ml", bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( <= )", "le_ml", bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( > )", "g_ml", bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( >= )", "ge_ml", bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( = )", "eq_ml", bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( == )", "peq_ml", bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( <> )", "neq_ml", bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( :: )", "cons_ml", bin_op (TPoly "'_a") (TList (TPoly "'_a")) (TList (TPoly "'_a"))
  ; "print_int", "print_int", Ast.TFunction (Ast.TInt, Ast.TUnit)
  ]
;;

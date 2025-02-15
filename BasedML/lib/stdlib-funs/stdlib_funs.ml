(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

type llvm_name = string
type name = string

type fun_type =
  | UserFun
  | SystemFun

type std_fun = llvm_name * name * fun_type * type_name

let bin_op arg1 arg2 ret = TFunction (arg1, TFunction (arg2, ret))

let stdlib_funs : std_fun list =
  [ "( + )", "plus_mlint", UserFun, bin_op TInt TInt TInt
  ; "( - )", "minus_mlint", UserFun, bin_op TInt TInt TInt
  ; "( * )", "mult_mlint", UserFun, bin_op TInt TInt TInt
  ; "( / )", "div_mlint", UserFun, bin_op TInt TInt TInt
  ; "( < )", "l_ml", UserFun, bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( <= )", "le_ml", UserFun, bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( > )", "g_ml", UserFun, bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( >= )", "ge_ml", UserFun, bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( = )", "eq_ml", UserFun, bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( == )", "peq_ml", UserFun, bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( <> )", "neq_ml", UserFun, bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; ( "( :: )"
    , "cons_ml"
    , UserFun
    , bin_op (TPoly "'_a") (TList (TPoly "'_a")) (TList (TPoly "'_a")) )
  ; "print_int", "print_int", UserFun, TFunction (TInt, TUnit)
  ; "", "get_box_field", SystemFun, TFunction (TPoly "'_a", TFunction (TInt, TPoly "'_b"))
  ; "", "check_box_tag", SystemFun, TFunction (TPoly "'_a", TFunction (TInt, TBool))
  ]
;;

(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

type llvm_name = string
type name = string

type fun_type =
  | UserFun
  | SystemFun

type std_fun = name * llvm_name * fun_type * type_name

let bin_op arg1 arg2 ret = TFunction (arg1, TFunction (arg2, ret))
let universal_function = TFunction (TPoly "'_a", TPoly "'_b")

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
  ; "( != )", "pneq_ml", UserFun, bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "print_int", "print_int", UserFun, TFunction (TInt, TUnit)
  ; "( && )", "", UserFun, bin_op TBool TBool TBool
  ; "( || )", "", UserFun, bin_op TBool TBool TBool
  ; ( "( :: )"
    , "mlrt_create_cons"
    , UserFun
    , bin_op (TPoly "'_a") (TList (TPoly "'_a")) (TList (TPoly "'_a")) )
  ; ( "get_field"
    , "mlrt_get_box_field"
    , SystemFun
    , TFunction (TPoly "'_a", TFunction (TInt, TPoly "'_b")) )
  ; ( "check_tag"
    , "mlrt_check_tag"
    , SystemFun
    , TFunction (TPoly "'_a", TFunction (TInt, TBool)) )
  ; "match_error", "mltr_match_error", SystemFun, TFunction (TUnit, TPoly "_a")
  ; "_create_tuple", "mlrt_create_tuple", SystemFun, TFunction (TInt, TPoly "'_vargs")
  ; "_create_empty_closure", "mlrt_create_empty_closure", SystemFun, universal_function
  ; "_apply_args_to_closure", "mlrt_apply_args_to_closure", SystemFun, universal_function
  ]
;;

(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast

type llvm_name = string
type name = string

type is_system =
  | UserFun
  | SystemFun

type is_vararg =
  | NotVararg
  | Vararg

type fun_type = is_system * is_vararg
type std_fun = name * llvm_name * fun_type * type_name

let default_type = UserFun, NotVararg
let bin_op arg1 arg2 ret = TFunction (arg1, TFunction (arg2, ret))
let universal_function = TFunction (TPoly "'_a", TPoly "'_b")

let stdlib_funs : std_fun list =
  [ "( + )", "plus_mlint", default_type, bin_op TInt TInt TInt
  ; "( - )", "minus_mlint", default_type, bin_op TInt TInt TInt
  ; "( * )", "mult_mlint", default_type, bin_op TInt TInt TInt
  ; "( / )", "div_mlint", default_type, bin_op TInt TInt TInt
  ; "( < )", "l_ml", default_type, bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( <= )", "le_ml", default_type, bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( > )", "g_ml", default_type, bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( >= )", "ge_ml", default_type, bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( = )", "eq_ml", default_type, bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( == )", "peq_ml", default_type, bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( <> )", "neq_ml", default_type, bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "( != )", "pneq_ml", default_type, bin_op (TPoly "'_a") (TPoly "'_a") TBool
  ; "print_int", "print_int", default_type, TFunction (TInt, TUnit)
  ; "( && )", "land_ml", default_type, bin_op TBool TBool TBool
  ; "( || )", "lor_ml", default_type, bin_op TBool TBool TBool
  ; ( "( :: )"
    , "mlrt_create_cons"
    , default_type
    , bin_op (TPoly "'_a") (TList (TPoly "'_a")) (TList (TPoly "'_a")) )
  ; ( "get_field"
    , "mlrt_get_box_field"
    , (SystemFun, NotVararg)
    , TFunction (TPoly "'_a", TFunction (TInt, TPoly "'_b")) )
  ; ( "check_tag"
    , "mlrt_check_tag"
    , (SystemFun, NotVararg)
    , TFunction (TPoly "'_a", TFunction (TInt, TBool)) )
  ; ( "match_error"
    , "mltr_match_error"
    , (SystemFun, NotVararg)
    , TFunction (TUnit, TPoly "_a") )
  ; ( "_create_tuple"
    , "mlrt_create_tuple"
    , (SystemFun, Vararg)
    , TFunction (TInt, TPoly "'_vargs") )
  ; ( "_create_empty_closure"
    , "mlrt_create_empty_closure"
    , (SystemFun, NotVararg)
    , universal_function )
  ; ( "_apply_args_to_closure"
    , "mlrt_apply_args_to_closure"
    , (SystemFun, Vararg)
    , TFunction (TPoly "_closure", TFunction (TInt, TPoly "_varargs")) )
  ; ( "_handle_global_vars"
    , "mlrt_handle_global_vars"
    , (SystemFun, Vararg)
    , TFunction (TPoly "_globs_count", TPoly "_varargs") )
  ; "compact", "mlrt_compact", (UserFun, NotVararg), TFunction (TUnit, TUnit)
  ; "_print_gc_info", "mlrt_print_gc_info", (UserFun, NotVararg), TFunction (TUnit, TUnit)
  ]
;;

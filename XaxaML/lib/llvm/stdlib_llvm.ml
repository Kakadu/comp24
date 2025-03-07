(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open! Llvm
open! Common_llvm
open! Symbol_map

let declare_func name llvm_t =
  let func = declare_function name llvm_t the_module in
  add_symbol name func;
  add_function_type name llvm_t;
  func
;;

let foreign_funcs =
  [ "create_int", function_type value_ptr_t [| i64_t |]
  ; "create_bool", function_type value_ptr_t [| bool_t |]
  ; "create_unit", function_type value_ptr_t [||]
  ; "get_int", function_type i64_t [| value_ptr_t |]
  ; "get_bool", function_type bool_t [| value_ptr_t |]
  ; "create_closure", function_type value_ptr_t [| function_ptr_t; i64_t |]
  ; "apply", function_type value_ptr_t [| value_ptr_t; value_ptr_t |]
  ; "print_int", function_type value_ptr_t [| value_ptr_t |]
  ; "print_bool", function_type value_ptr_t [| value_ptr_t |]
  ; "create_empty_list", function_type value_ptr_t [||]
  ; "list_cons", function_type value_ptr_t [| value_ptr_t; value_ptr_t |]
  ; "#list_hd", function_type value_ptr_t [| value_ptr_t |]
  ; "#list_tl", function_type value_ptr_t [| value_ptr_t |]
  ; "#list_length", function_type value_ptr_t [| value_ptr_t |]
  ; "create_tuple", var_arg_function_type value_ptr_t [| i32_t |]
  ; "#unpack_tuple", function_type value_ptr_t [| value_ptr_t; value_ptr_t |]
  ; "#match_failure", function_type value_ptr_t [| value_ptr_t; value_ptr_t |]
  ; "~+", function_type value_ptr_t [| value_ptr_t |]
  ; "~-", function_type value_ptr_t [| value_ptr_t |]
  ; "+", function_type value_ptr_t [| value_ptr_t; value_ptr_t |]
  ; "-", function_type value_ptr_t [| value_ptr_t; value_ptr_t |]
  ; "*", function_type value_ptr_t [| value_ptr_t; value_ptr_t |]
  ; "/", function_type value_ptr_t [| value_ptr_t; value_ptr_t |]
  ; "&&", function_type value_ptr_t [| value_ptr_t; value_ptr_t |]
  ; "||", function_type value_ptr_t [| value_ptr_t; value_ptr_t |]
  ; ">", function_type value_ptr_t [| value_ptr_t; value_ptr_t |]
  ; "<", function_type value_ptr_t [| value_ptr_t; value_ptr_t |]
  ; "<=", function_type value_ptr_t [| value_ptr_t; value_ptr_t |]
  ; ">=", function_type value_ptr_t [| value_ptr_t; value_ptr_t |]
  ; "=", function_type value_ptr_t [| value_ptr_t; value_ptr_t |]
  ; "<>", function_type value_ptr_t [| value_ptr_t; value_ptr_t |]
  ; "==", function_type value_ptr_t [| value_ptr_t; value_ptr_t |]
  ; "!=", function_type value_ptr_t [| value_ptr_t; value_ptr_t |]
  ]
;;

let declare_foreign_function (name, llvm_t) =
  ignore (declare_func (map_stdlib_to_runtime name) llvm_t)
;;

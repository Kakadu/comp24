(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm
open LlvmBasic
module RuntimeEnv = Map.Make (String)

let runtime_functions =
  let basic_value_creation =
    [ "ct_int_v", function_type ptr_ty [| i32_ty |]
    ; "ct_bool_v", function_type ptr_ty [| bool_ty |]
    ; "ct_str_v", function_type ptr_ty [| ptr_ty |]
    ; "ct_char_v", function_type ptr_ty [| i8_ty |]
    ; "ct_unit_v", function_type ptr_ty [||]
    ]
  in
  let arithmetic =
    [ "multip", function_type ptr_ty [| ptr_ty; ptr_ty |]
    ; "division", function_type ptr_ty [| ptr_ty; ptr_ty |]
    ; "divRemainder", function_type ptr_ty [| ptr_ty; ptr_ty |]
    ; "plus", function_type ptr_ty [| ptr_ty; ptr_ty |]
    ; "minus", function_type ptr_ty [| ptr_ty; ptr_ty |]
    ]
  in
  let unary_operations =
    [ "uplus", function_type ptr_ty [| ptr_ty |]
    ; "uminus", function_type ptr_ty [| ptr_ty |]
    ; "unot", function_type ptr_ty [| ptr_ty |]
    ]
  in
  let logical_operations =
    [ "bOr", function_type ptr_ty [| ptr_ty; ptr_ty |]
    ; "bAnd", function_type ptr_ty [| ptr_ty; ptr_ty |]
    ; "check_cond", function_type bool_ty [| ptr_ty |];
    ]
  in
  let comparison_operations =
    [ "feq", function_type ptr_ty [| ptr_ty; ptr_ty |] (* = *)
    ; "leq", function_type ptr_ty [| ptr_ty; ptr_ty |] (* == *)
    ; "fneq", function_type ptr_ty [| ptr_ty; ptr_ty |] (* <> *)
    ; "lneq", function_type ptr_ty [| ptr_ty; ptr_ty |] (* != *)
    ; "lt", function_type ptr_ty [| ptr_ty; ptr_ty |] (* < *)
    ; "le", function_type ptr_ty [| ptr_ty; ptr_ty |] (* <= *)
    ; "gt", function_type ptr_ty [| ptr_ty; ptr_ty |] (* > *)
    ; "ge", function_type ptr_ty [| ptr_ty; ptr_ty |] (* >= *)
    ]
  in
  let stdlib_functions =
    [ "print_int", function_type void_ty [| ptr_ty |]
    ; "print_bool", function_type void_ty [| ptr_ty |]
    ; "print_string", function_type void_ty [| ptr_ty |]
    ; "print_char", function_type void_ty [| ptr_ty |]
    ]
  in
  let closure_functions =
    [ "ct_closure", function_type ptr_ty [| ptr_ty; i32_ty |]
    ; "invoke_closure_with_args", var_arg_function_type ptr_ty [| ptr_ty; i32_ty |]
    ; "invoke_closure", function_type ptr_ty [| ptr_ty |]
    ]
  in
  let pattern_matching_functions =
    [ "pattern_matching_failure", function_type void_ty [||] ]
  in
  basic_value_creation
  @ arithmetic
  @ unary_operations
  @ logical_operations
  @ comparison_operations
  @ stdlib_functions
  @ closure_functions
  @ pattern_matching_functions
;;

let unsupported_runtime_functions =
  [ "ct_tuple", var_arg_function_type ptr_ty [| i32_ty |]
  ; "unpack_tuple", function_type ptr_ty [| ptr_ty; ptr_ty |]
  ; "ct_empty_list", function_type ptr_ty [||]
  ; "add_to_list", function_type ptr_ty [| ptr_ty; ptr_ty |]
  ; "list_hd", function_type ptr_ty [| ptr_ty |]
  ; "list_tl", function_type ptr_ty [| ptr_ty |]
  ; "list_length", function_type ptr_ty [| ptr_ty |]
  ; "pattern_matching_failure", function_type void_ty [||]
  ]
;;

let name_to_runtime_mapping = function
  | "( + )" -> "plus"
  | "( - )" -> "minus"
  | "( * )" -> "multip"
  | "( / )" -> "division"
  | "( % )" -> "divRemainder"
  | "U+" -> "uplus"
  | "U-" -> "uminus"
  | "UNot" -> "unot"
  | "( && )" -> "bAnd"
  | "( || )" -> "bOr"
  | "( = )" -> "feq"
  | "( == )" -> "leq"
  | "( <> )" -> "fneq"
  | "( != )" -> "lneq"
  | "( < )" -> "lt"
  | "( <= )" -> "le"
  | "( > )" -> "gt"
  | "( >= )" -> "ge"
  | other -> other
;;

let name_to_runtime_mapping_opt name = Some (name_to_runtime_mapping name)

let runtimeEnvironment =
  List.fold_left
    (fun acc name -> RuntimeEnv.add name (name_to_runtime_mapping name) acc)
    RuntimeEnv.empty
    Common.StartEnvironment.start_identifiers
;;

let unsupportedRuntimeEnvironment =
  List.fold_left
    (fun acc (name, _) -> RuntimeEnv.add name (name_to_runtime_mapping name) acc)
    RuntimeEnv.empty
    unsupported_runtime_functions
;;

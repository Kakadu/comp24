(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open! Llvm
open! Llvm_target

(* В начале компилятора, при создании модуля *)
let ctx = global_context ()
let the_module = create_module ctx "XAXAML"
let target_triple = Llvm_target.Target.default_triple ()
let () = Llvm.set_target_triple target_triple the_module

let () =
  Llvm_all_backends.initialize ();
  let target = Llvm_target.Target.by_triple target_triple in
  let machine = Llvm_target.TargetMachine.create ~triple:target_triple target in
  let data_layout = Llvm_target.TargetMachine.data_layout machine in
  Llvm.set_data_layout (Llvm_target.DataLayout.as_string data_layout) the_module
;;

let builder = builder ctx

(* Базовые типы *)
let i64_t = i64_type ctx
let i32_t = i32_type ctx
let i8_t = i8_type ctx
let bool_t = i1_type ctx
let void_t = void_type ctx

(* Определение Value* типа *)
let value_ptr_t = pointer_type ctx

(* Определение FunctionPtr типа *)
let function_ptr_t = pointer_type ctx

let map_stdlib_to_runtime = function
  | "+" -> "add"
  | "-" -> "sub"
  | "*" -> "mul"
  | "/" -> "div_op"
  | "=" -> "eq"
  | "<>" -> "ne"
  | "<" -> "lt"
  | ">" -> "gt"
  | "<=" -> "le"
  | ">=" -> "ge"
  | "&&" -> "and_op"
  | "||" -> "or_op"
  | "~+" -> "uplus"
  | "~-" -> "uminus"
  | "!=" -> "phys_ne"
  | "==" -> "phys_eq"
  | "#list_hd" -> "list_hd"
  | "#list_tl" -> "list_tl"
  | "#list_length" -> "list_length"
  | "#unpack_tuple" -> "unpack_tuple"
  | "#match_failure" -> "match_failure"
  | other -> other
;;

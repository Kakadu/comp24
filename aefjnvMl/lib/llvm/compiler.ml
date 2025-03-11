(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm
open Llvm_generator

let compile_llvm2ll out_name prog =
  let module CurCTX = struct
    let ctx = Llvm.global_context ()
    let g_module = Llvm.create_module ctx "aefjnv_ml"
    let void_t = void_type ctx
    let i64_t = i64_type ctx
  end
  in
  let module Compiler = LlvmCompiler (CurCTX) in
  match Compiler.compile_llvm_common prog with
  | _, Ok _ -> print_module out_name CurCTX.g_module
  | _, Error msg -> Format.printf "COMPILE_ERROR: %s\n" msg
;;

let compile_program prog =
  let module CurCTX = struct
    let ctx = Llvm.global_context ()
    let g_module = Llvm.create_module ctx "aefjnv_ml"
    let void_t = void_type ctx
    let i64_t = i64_type ctx
  end
  in
  let module Compiler = LlvmCompiler (CurCTX) in
  match Compiler.compile_llvm_common prog with
  | _, Ok _ -> Ok (string_of_llmodule CurCTX.g_module)
  | _, Error msg -> Error (Common.Errors.llvm_error msg)
;;

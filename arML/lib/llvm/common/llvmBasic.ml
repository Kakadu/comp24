(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm

let context = global_context ()
let the_module = create_module context "arML"
let builder = builder context
let i64_ty = Llvm.i64_type context
let i32_ty = Llvm.i32_type context
let i8_ty = Llvm.i8_type context
let bool_ty = Llvm.i1_type context
let ptr_ty = Llvm.pointer_type context
let void_ty = Llvm.void_type context

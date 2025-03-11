(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm
open LlvmBasic
open EnvironmentSearchers
open Anf.Anftree

let rec compile_immut_expression runtime env = function
  | IConstant const ->
    (match const with
     | CInt i ->
       let f_value, ty = get_func_info runtime "ct_int_v" in 
       build_call ty f_value [| const_int i32_ty i |] "val_int" builder
     | CBool b ->
       let f_value, ty = get_func_info runtime "ct_bool_v" in
       let v = if b then 1 else 0 in
       build_call ty f_value [| const_int bool_ty v |] "val_bool" builder
     | CString s ->
       let f_value, ty = get_func_info runtime "ct_str_v" in
       let str_global = build_global_stringptr s "str" builder in
       build_call ty f_value [| str_global |] "val_str" builder
     | CChar c ->
       let f_value, ty = get_func_info runtime "ct_int_v" in
       let char_as_int = int_of_char c in
       build_call ty f_value [| const_int i32_ty char_as_int |] "val_char" builder
     | CUnit ->
       let f_value, ty = get_func_info runtime "ct_int_v" in
       build_call ty f_value [| const_int i32_ty 0 |] "val_unit" builder)
  | IIdentifier _ -> assert false
  | ITuple _ -> failwith "Tuples unsupported in llvm codegen"
  | IEmptyList -> failwith "List unsupported in llvm codegen"

and compile_complex_expression runtime env = function
  | CAtom a -> compile_immut_expression runtime env a
  | CApplication _ -> assert false
  | CIfThenElse _ -> assert false
  | CTyped (c, _) -> compile_complex_expression runtime env c
  | CListConstructor _ -> failwith "List unsupported in llvm codegen"
;;
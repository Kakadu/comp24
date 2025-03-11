(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm
open LlvmBasic
open EnvironmentSearchers
open Runtime
open Anf.Anftree
open RuntimeClosures

let rec compile_immut_expression runtime env = function
  | IConstant const -> compile_constant_expressions runtime const
  | IIdentifier (Id n) -> compile_identifier_expression runtime env n
  | ITuple _ -> failwith "Tuples unsupported in llvm codegen"
  | IEmptyList -> failwith "List unsupported in llvm codegen"

and compile_complex_expression runtime env = function
  | CAtom a -> compile_immut_expression runtime env a
  | CApplication (f, arg, args) -> compile_func_call runtime env f (arg :: args)
  | CIfThenElse (c, b1, b2) -> compile_if_then_else runtime env c b1 b2
  | CTyped (c, _) -> compile_complex_expression runtime env c
  | CListConstructor _ -> failwith "List unsupported in llvm codegen"

and compile_anf_expression runtime env = function
  | AComplex c -> compile_complex_expression runtime env c
  | ALetIn (Id n, e1, e2) ->
    compile_anf_expression
      runtime
      (RuntimeEnv.add n (compile_complex_expression runtime env e1) env)
      e2

and compile_if_then_else runtime env cond_expr then_expr else_expr =
  let cond_value = compile_immut_expression runtime env cond_expr in
  let bool_func, bool_func_type = get_func_info runtime "check_cond" in
  let cond_bool =
    build_call bool_func_type bool_func [| cond_value |] "cond_bool" builder
  in
  let current_func = block_parent (insertion_block builder) in
  let then_block = append_block context "then_block" current_func in
  let else_block = append_block context "else_block" current_func in
  let merge_block = append_block context "merge_block" current_func in
  build_cond_br cond_bool then_block else_block builder |> ignore;
  position_at_end then_block builder;
  let then_result = compile_anf_expression runtime env then_expr in
  let then_end_block = Llvm.insertion_block builder in
  build_br merge_block builder |> ignore;
  position_at_end else_block builder;
  let else_result = compile_anf_expression runtime env else_expr in
  let else_end_block = Llvm.insertion_block builder in
  build_br merge_block builder |> ignore;
  position_at_end merge_block builder;
  build_phi
    [ then_result, then_end_block; else_result, else_end_block ]
    "if_expr_result"
    builder

and compile_constant_expressions runtime = function
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
    build_call ty f_value [| const_int i32_ty 0 |] "val_unit" builder

and compile_identifier_expression runtime env name =
  let wrap_function runtime func =
    let arg_count = Array.length (params func) in
    let closure = create_fun_closure runtime func arg_count in
    if arg_count = 0 then invoke_closure runtime closure else closure
  in
  let resolve_function runtime env name =
    let value = get_func_value runtime env name in
    match Llvm.classify_value value with
    | ValueKind.Function -> wrap_function runtime value
    | _ -> value
  in
  resolve_function runtime env name

and compile_func_call runtime env func_expr arg_exprs =
  let compiled_func =
    match func_expr with
    | IIdentifier (Id func_name) -> compile_identifier_expression runtime env func_name
    | _ -> failwith "Error: Invalid function call"
  in
  let compiled_args = List.map (compile_immut_expression runtime env) arg_exprs in
  let closure_func, closure_type = get_func_info runtime "invoke_closure_with_args" in
  let closure_args =
    Array.of_list
      (compiled_func :: const_int i32_ty (List.length compiled_args) :: compiled_args)
  in
  build_call closure_type closure_func closure_args "closure_call_result" builder
;;

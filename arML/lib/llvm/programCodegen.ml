(* Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm
open LlvmBasic
open Runtime
open Environment
open ExpressionCodegen
open Ast.AbstractSyntaxTree
open Anf.Anftree

let compile_program program =
  let initial_env =
    List.fold_left
      (fun env decl ->
         let funcs =
           match decl with
           | ADOrdinary func_def -> [ func_def ]
           | ADRecursive (first_func, other_funcs) -> first_func :: other_funcs
         in
         declare_program_functions funcs env)
      Env.empty
      program
  in
  let process_function (Id func_name, arg_list, body_expr) =
    let param_names = List.map (fun (Id arg) -> arg) arg_list in
    let func_ref =
      match Env.find_opt func_name initial_env with
      | Some func -> func
      | None -> failwith "Error: Unbound function"
    in
    let entry_block = append_block context "entry" func_ref in
    Llvm.position_at_end entry_block builder;
    let updated_env =
      List.fold_left2
        (fun acc_env param_name param_val -> Env.add param_name param_val acc_env)
        initial_env
        param_names
        (Array.to_list (Llvm.params func_ref))
    in
    let return_value = compile_anf_expression (declare_functions_in_env RuntimeEnv.empty Runtime.runtime_functions) updated_env body_expr in
    let return_instr =
      if func_name = "main"
      then Llvm.build_ret (const_int i32_ty 0) builder
      else Llvm.build_ret return_value builder
    in
    ignore return_instr
  in
  List.iter
    (fun decl ->
       let functions =
         match decl with
         | ADOrdinary func -> [ func ]
         | ADRecursive (first_func, other_funcs) -> first_func :: other_funcs
       in
       List.iter process_function functions)
    program;
  string_of_llmodule the_module
;;

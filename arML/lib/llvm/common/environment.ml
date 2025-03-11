(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open LlvmBasic
open Ast.AbstractSyntaxTree
module Env = Map.Make (String)

let rec declare_functions_in_env env = function
  | [] -> env
  | (name, typ) :: rest ->
    let ext_func = Llvm.declare_function name typ the_module in
    let new_env = Env.add name ext_func env in
    declare_functions_in_env new_env rest
;;

let declare_program_functions funcs env =
  let funcs_without_expr =
    List.map
      (fun (Id name, args, _) ->
        let args = List.map (fun (Id a) -> a) args in
        name, fun_name_to_fun_type_mapping args name)
      funcs
  in
  declare_functions_in_env env funcs_without_expr
;;

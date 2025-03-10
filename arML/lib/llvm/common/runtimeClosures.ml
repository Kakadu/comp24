(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm
open LlvmBasic
open EnvironmentSearchers

let create_fun_closure runtime id_val args_num =
  let func, typ = get_func_info runtime "ct_closure" in
  let closure =
    build_call typ func [| id_val; const_int i32_ty args_num |] "closure" builder
  in
  closure
;;

let call_fun runtime closure =
  let func, typ = get_func_info runtime "call_fun" in
  build_call typ func [| closure |] "fun_call" builder
;;

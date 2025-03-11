(** Copyright 2024-2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Llvm
open Llvm_target
open Utils.Predefined_ops

(*Env init*)
let context = global_context ()
let my_module = create_module context "BDSML"
let target_triple = Target.default_triple ()
let () = set_target_triple target_triple my_module
let () = Llvm_all_backends.initialize ()

let () =
  let target = Target.by_triple target_triple in
  let machine = TargetMachine.create ~triple:target_triple target in
  let data_layout = TargetMachine.data_layout machine in
  set_data_layout (DataLayout.as_string data_layout) my_module
;;

(*Types init*)
let builder = builder context
let int_t = i64_type context
let char_t = i8_type context
let bool_t = i1_type context
let void_t = void_type context
let ptr_t = pointer_type context

let build_list_type element_type =
  let node_type = struct_type context [| element_type; ptr_t |] in
  node_type
;;

(*Predefined*)

let predefined_funcs =
  [ "create_int", function_type ptr_t [| int_t |]
  ; "create_bool", function_type ptr_t [| bool_t |]
  ; "create_unit", function_type ptr_t [||]
  ; "create_char", function_type ptr_t [| char_t |]
  ; "create_string", var_arg_function_type ptr_t [||]
  ; "create_apply", var_arg_function_type ptr_t [||]
  ; "create_tuple", var_arg_function_type ptr_t [| int_t |]
  ; "create_list", function_type ptr_t [| ptr_t; ptr_t |]
  ; "create_empty_list", function_type ptr_t [||]
  ; "create_cons", function_type ptr_t [| bool_t; ptr_t |]
  ; "create_closure", function_type ptr_t [| ptr_t; int_t |]
  ; "create_apply", function_type ptr_t [| ptr_t; ptr_t |]
  ; "get_int", function_type int_t [| ptr_t |]
  ; "get_bool", function_type bool_t [| ptr_t |]
  ; "get_char", function_type char_t [| ptr_t |]
  ]
;;

(*Hashtable*)

let variable_value_table : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 10

let variable_type_table : (string, Llvm.lltype) Hashtbl.t =
  Hashtbl.create (List.length predefined_funcs * 2)
;;

let find t key =
  match Hashtbl.find_opt t key with
  | Some v -> v
  | None -> failwith ("Couldn't find value for key" ^ key)
;;

let predefined_init () =
  ignore
  @@ List.map
       (fun (str, t) ->
         let () = ignore @@ declare_function str t my_module in
         Hashtbl.add variable_type_table str t)
       predefined_funcs;
  ignore
  @@ List.map
       (fun op ->
         let t =
           match op.alt_name with
           | "print_int" -> op.llvm_t void_t ptr_t
           | _ -> op.llvm_t ptr_t ptr_t
         in
         let () = ignore @@ declare_function op.alt_name t my_module in
         Hashtbl.add variable_type_table op.alt_name t)
       predefine_operators
;;

open Llvm
open Llvm_target

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

let predefined =
  [ "create_int", function_type ptr_t [| int_t |]
  ; "create_bool", function_type ptr_t [| bool_t |]
  ; "create_unit", function_type ptr_t [||]
  ; "create_char", function_type ptr_t [| char_t |]
  ; "create_string", var_arg_function_type ptr_t [||]
  ; "create_apply", var_arg_function_type ptr_t [||]
  ; "create_tuple", var_arg_function_type ptr_t [| int_t |]
  ; "create_list", function_type ptr_t [| ptr_t; ptr_t |]
  ; "create_list", function_type ptr_t [| ptr_t; ptr_t |]
  ; "create_empty_list", function_type ptr_t [||]
  ; "create_cons", function_type ptr_t [| bool_t; ptr_t |]
  ; "create_closure", function_type ptr_t [| ptr_t; int_t |]
  ; "get_int", function_type int_t [| ptr_t |]
  ; "get_bool", function_type bool_t [| ptr_t |]
  ; "get_char", function_type char_t [| ptr_t |]
  ; "__op_plus", function_type int_t [| int_t; int_t |]
  ; "__op_minus", function_type int_t [| int_t; int_t |]
  ; "__op_mult", function_type int_t [| int_t; int_t |]
  ; "__op_div", function_type int_t [| int_t; int_t |]
  ; "__op_neg", function_type int_t [| int_t |]
  ; "__op_pos", function_type int_t [| int_t |]
  ; "not", function_type bool_t [| bool_t |]
  ; "__op_gt", function_type bool_t [| ptr_t; ptr_t |]
  ; "__op_ge", function_type bool_t [| ptr_t; ptr_t |]
  ; "__op_lt", function_type bool_t [| ptr_t; ptr_t |]
  ; "__op_le", function_type bool_t [| ptr_t; ptr_t |]
  ; "__op_eq", function_type bool_t [| ptr_t; ptr_t |]
  ; "__op_neq", function_type bool_t [| ptr_t; ptr_t |]
  ; "__op_or", function_type bool_t [| bool_t; bool_t |]
  ; "__op_and", function_type bool_t [| bool_t; bool_t |]
  ; "__op_phys_eq", function_type bool_t [| ptr_t; ptr_t |]
  ; "print_int", function_type void_t [| int_t |]
  ]
;;

(*Hashtable*)

let variable_value_table : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 10
let variable_type_table : (string, Llvm.lltype) Hashtbl.t = Hashtbl.create 10

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
       predefined
;;

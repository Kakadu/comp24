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

(*Hashtable*)

let variable_value_table : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 10

(*Predefined*)

let predefined =
  [ "__op_plus", function_type int_t [| int_t; int_t |]
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

let predefined_init () =
  ignore @@ List.map (fun (str, t) -> declare_function str t my_module) predefined
;;

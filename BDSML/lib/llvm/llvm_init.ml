open Llvm
open Llvm_target
open Middleend.Anf_ast

(*Env init*)
let context = global_context ()
let my_module = create_module context "BDSML"
let target_triple = Llvm_target.Target.default_triple ()
let () = set_target_triple target_triple my_module
let () = Llvm_all_backends.initialize ()

let () =
  let target = Llvm_target.Target.by_triple target_triple in
  let machine = Llvm_target.TargetMachine.create ~triple:target_triple target in
  let data_layout = Llvm_target.TargetMachine.data_layout machine in
  set_data_layout (Llvm_target.DataLayout.as_string data_layout) my_module
;;

(*Types init*)
let builder = builder context
let int_t = i64_type context
let char_t = i8_type context
let bool_t = i1_type context
let void_t = void_type context
let ptr_t = pointer_type context

(*Hashtables*)

let variable_value_table : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 10

(*Builders functions*)

let rec build_list = function
  | AExp_construct ("[]", None) -> []
  | AExp_construct ("::", Some (AExp_tuple [ head; tail ])) -> head :: build_list tail
  | _ -> failwith "Not a list"
;;

let build_none = const_struct context [| const_int bool_t 0; const_null ptr_t |]
let build_some value = const_struct context [| const_int bool_t 1; value |]

let rec build_construct = function
  | AExp_construct ("[]", None) -> []
  | AExp_construct ("::", Some (AExp_tuple [ head; tail ])) -> head :: build_list tail
  | _ -> failwith "Not a list"
;;

let build_ident name =
  match Hashtbl.find_opt variable_value_table name with
  | Some value -> build_load ptr_t value name builder
  | None -> failwith ("Unknown variable: " ^ name)
;;

(** Copyright 2024-2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Llvm
open Llvm_init
open Middleend.Anf_ast
open Parser.Ast

let lookup_func str =
  match lookup_function str my_module with
  | Some f -> f
  | None -> failwith "func defenetly should be defined"
;;

let compile_simple_type t v =
  let name = "create_" ^ t in
  let func = lookup_func name in
  let ft = find variable_type_table name in
  build_call ft func v ("boxed_" ^ t) builder
;;

let rec compile_aexpr = function
  | AExp_constant c ->
    (match c with
     | Const_int i -> compile_simple_type "int" [| const_int int_t i |]
     | Const_bool b ->
       compile_simple_type "bool" [| const_int bool_t (if b then 1 else 0) |]
     | Const_char c -> compile_simple_type "char" [| const_int char_t @@ Char.code c |]
     | Const_string s -> compile_simple_type "string" [| const_stringz context s |]
     | Const_unit -> compile_simple_type "unit" [||])
  | AExp_tuple elems ->
    let args = List.map compile_aexpr elems in
    let len = List.length args in
    compile_simple_type "tuple" (Array.of_list (const_int int_t len :: args))
  | AExp_construct ("::", Some (AExp_tuple [ head; tail ])) ->
    let head' = compile_aexpr head in
    let tail' = compile_aexpr tail in
    compile_simple_type "list" [| head'; tail' |]
  | AExp_construct ("[]", _) -> compile_simple_type "empty_list" [||]
  | AExp_construct ("None", _) ->
    compile_simple_type "cons" [| const_int bool_t 0; const_null ptr_t |]
  | AExp_construct ("Some", x) ->
    (match x with
     | None -> failwith "Impossible"
     | Some x ->
       let x' = compile_aexpr x in
       compile_simple_type "cons" [| const_int bool_t 0; x' |])
  | AExp_construct (_, _) -> failwith "Not implemented"
  | AExp_ident id ->
    let value = find variable_value_table id in
    maybe_closure (build_load ptr_t value id builder)

and maybe_closure value =
  match classify_value value with
  | ValueKind.Function ->
    let arity = Array.length (params value) in
    let function_ptr = build_bitcast value ptr_t "func_ptr_cast" builder in
    compile_simple_type "closure" [| function_ptr; const_int int_t arity |]
  | _ -> value
;;

let rec compile_cexpr = function
  | CExp_if (ae, l1, l2) ->
    let cond_val = compile_aexpr ae in
    let bool_func = lookup_func "get_bool" in
    let bool_func_type = find variable_type_table "get_bool" in
    let bool_val =
      build_call bool_func_type bool_func [| cond_val |] "cond_bool" builder
    in
    let current_func = block_parent (insertion_block builder) in
    let then_block = append_block context "then" current_func in
    let else_block = append_block context "else" current_func in
    let merge_block = append_block context "merge" current_func in
    ignore (build_cond_br bool_val then_block else_block builder);
    position_at_end then_block builder;
    let then_val = compile_lexpr l1 in
    ignore (build_br merge_block builder);
    let then_bb = insertion_block builder in
    position_at_end else_block builder;
    let else_val = compile_lexpr l2 in
    ignore (build_br merge_block builder);
    let else_bb = insertion_block builder in
    position_at_end merge_block builder;
    build_phi [ then_val, then_bb; else_val, else_bb ] "ite_result" builder
  | CExp_apply (s, ael) ->
    let func = lookup_function s my_module in
    (match func with
     | None -> failwith ("Undefined function: " ^ s)
     | Some fn ->
       let fn' = maybe_closure fn in
       let ael' = Array.of_list (List.map compile_aexpr ael) in
       compile_simple_type
         "apply"
         (Array.append
            [| fn'; compile_simple_type "int" [| const_int int_t (Array.length ael') |] |]
            ael'))
  | CExp_atom ae -> compile_aexpr ae

and compile_lexpr = function
  | LLet_in (s, c, l) ->
    let value = compile_cexpr c in
    Hashtbl.add variable_value_table s value;
    compile_lexpr l
  | LComplex c -> compile_cexpr c
;;

let compile_func f =
  let name, args, body = f in
  let () =
    ignore
    @@ declare_function
         name
         (function_type ptr_t (Array.init (List.length args) (fun _ -> ptr_t)))
         my_module
  in
  let func = lookup_func name in
  let entry = append_block context "entry" func in
  position_at_end entry builder;
  let func_params = params func in
  List.iteri
    (fun i arg_name ->
      let param = Array.get func_params i in
      set_value_name arg_name param;
      Hashtbl.add variable_value_table arg_name param)
    args;
  let body_value = compile_lexpr body in
  let _ = build_ret body_value builder in
  func
;;

let compile_funcs = function
  | AbsStr_func f -> ignore @@ compile_func f
  | AbsStr_value_rec fl -> List.iter (fun f -> ignore @@ compile_func f) fl
  | AbsStr_value (s, _) ->
    let global_var = define_global s (const_null ptr_t) my_module in
    Hashtbl.add variable_value_table s global_var
;;

let compile_values = function
  | AbsStr_value (s, l) ->
    let global_var = find variable_value_table s in
    let compiled_val = compile_lexpr l in
    ignore (build_store compiled_val global_var builder);
    Hashtbl.add variable_value_table s compiled_val
  | _ -> ()
;;

let declare_everything anf = List.iter (fun expr -> compile_funcs expr) anf
let declare_values anf = List.iter (fun expr -> compile_values expr) anf

let compile_main program =
  let entry =
    append_block
      context
      "entry"
      (declare_function "main" (function_type int_t [||]) my_module)
  in
  let () = position_at_end entry builder in
  let () = declare_values program in
  ignore (build_ret (const_int int_t 0) builder)
;;

let compile_program ?(verbose = false) program =
  let () = predefined_init () in
  let () = declare_everything program in
  let () = compile_main program in
  print_module "out.ll" my_module;
  if verbose then dump_module my_module
;;

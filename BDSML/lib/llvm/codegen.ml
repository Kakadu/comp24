open Llvm
open Llvm_init
open Middleend.Anf_ast
open Parser.Ast

let rec compile_aexpr =
  let rec build_list = function
    | AExp_construct ("[]", None) -> []
    | AExp_construct ("::", Some (AExp_tuple [ head; tail ])) -> head :: build_list tail
    | _ -> failwith "Not a list"
  in
  function
  | AExp_constant c ->
    (match c with
     | Const_int i -> const_int int_t i
     | Const_bool b -> const_int bool_t (if b then 1 else 0)
     | Const_char c -> const_int char_t @@ Char.code c
     | Const_string s -> const_string context s
     | Const_unit ->
       let fn_t = function_type void_t [||] in
       define_function "unit_t" fn_t my_module)
  | AExp_tuple elems ->
    let types = List.map compile_aexpr elems in
    const_struct context (Array.of_list types)
  | AExp_construct ("::", _) as l ->
    let l' = build_list l in
    let types = List.map compile_aexpr l' in
    const_struct context (Array.of_list types)
  | AExp_construct ("[]", _) -> const_struct context (Array.of_list [])
  | AExp_construct ("None", _) ->
    const_struct context [| const_int bool_t 0; const_null ptr_t |]
  | AExp_construct ("Some", x) ->
    (match x with
     | None -> failwith "Impossible"
     | Some x -> const_struct context [| const_int bool_t 1; compile_aexpr x |])
  | AExp_construct (_, _) -> failwith "Not implemented"
  | AExp_ident id ->
    (match Hashtbl.find_opt variable_value_table id with
     | Some value -> build_load ptr_t value id builder
     | None -> failwith ("Unknown variable: " ^ id))
;;

let rec compile_cexpr = function
  | CExp_if (ae, l1, l2) ->
    let ae' = compile_aexpr ae in
    let func = block_parent (insertion_block builder) in
    let then_block = append_block context "then" func in
    let else_block = append_block context "else" func in
    let merge_block = append_block context "merge" func in
    let () = ignore (build_cond_br ae' then_block else_block builder) in
    let () = position_at_end then_block builder in
    let then_val = compile_lexpr l1 in
    let () = ignore (build_br merge_block builder) in
    let then_bb = insertion_block builder in
    let () = position_at_end else_block builder in
    let else_val = compile_lexpr l2 in
    let () = ignore (build_br merge_block builder) in
    let else_bb = insertion_block builder in
    let () = position_at_end merge_block builder in
    build_phi [ then_val, then_bb; else_val, else_bb ] "ite_result" builder
  | CExp_apply (s, ael) ->
    let func = lookup_function s my_module in
    (match func with
     | None -> failwith ("Undefined function: " ^ s)
     | Some fn ->
       let compiled_args = List.map compile_aexpr ael in
       build_call (type_of fn) fn (Array.of_list compiled_args) "call_tmp" builder)
  | CExp_atom ae -> compile_aexpr ae

and compile_lexpr = function
  | LLet_in (s, c, l) ->
    let value = compile_cexpr c in
    let var_alloca = build_alloca (type_of value) s builder in
    let _ = build_store value var_alloca builder in
    Hashtbl.add variable_value_table s var_alloca;
    let body_val = compile_lexpr l in
    Hashtbl.remove variable_value_table s;
    body_val
  | LComplex c -> compile_cexpr c
;;

let compile_func f =
  let name, args, body = f in
  let param_types = Array.make (List.length args) int_t in
  let func_type = function_type int_t param_types in
  let func = define_function name func_type my_module in
  let entry_block = append_block context "entry" func in
  let builder = builder_at_end context entry_block in
  List.iteri
    (fun i arg_name ->
      let param = Llvm.param func i in
      let param_alloca = Llvm.build_alloca (Llvm.type_of param) arg_name builder in
      let _ = Llvm.build_store param param_alloca builder in
      Hashtbl.add variable_value_table arg_name param_alloca)
    args;
  let body_value = compile_lexpr body in
  let _ = Llvm.build_ret body_value builder in
  func
;;

let compile_func f =
  let name, args, body = f in
  let param_types = Array.make (List.length args) int_t in
  let func_type = function_type int_t param_types in
  let func = define_function name func_type my_module in
  let entry_block = append_block context "entry" func in
  let builder = builder_at_end context entry_block in
  List.iteri
    (fun i arg_name ->
      let param = Llvm.param func i in
      let param_alloca = Llvm.build_alloca (Llvm.type_of param) arg_name builder in
      let _ = Llvm.build_store param param_alloca builder in
      Hashtbl.add variable_value_table arg_name param_alloca)
    args;
  let body_value = compile_lexpr body in
  let _ = Llvm.build_ret body_value builder in
  func
;;

let compile_funcs = function
  | AbsStr_func f -> ignore @@ compile_func f
  | AbsStr_value_rec fl ->
    let () =
      List.iter
        (fun (name, args, _) ->
          let param_types = Array.make (List.length args) int_t in
          let func_type = function_type int_t param_types in
          ignore (declare_function name func_type my_module))
        fl
    in
    ignore @@ List.map (fun func -> compile_func func) fl
  | _ -> ()
;;

let compile_values = function
  | AbsStr_value (s, l) ->
    let value = compile_lexpr l in
    let var_alloca = build_alloca (type_of value) s builder in
    let _ = build_store value var_alloca builder in
    let () = Hashtbl.add variable_value_table s var_alloca in
    ignore @@ value
  | _ -> ()
;;

let declare_functions anf = List.iter (fun expr -> compile_funcs expr) anf
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
  let () = declare_functions program in
  let () = compile_main program in
  print_module "out.ll" my_module;
  if verbose then dump_module my_module
;;

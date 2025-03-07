open Llvm
open Containers

let context = global_context ()
let the_module = create_module context "ObaML"
let target_triple = Llvm_target.Target.default_triple ();;

Llvm.set_target_triple target_triple the_module

let builder = builder context
let i64_type = Llvm.i64_type context
let i32_type = Llvm.i32_type context
let i8_type = Llvm.i8_type context
let bool_type = Llvm.i1_type context
let ptr_type = Llvm.pointer_type context
let void_type = Llvm.void_type context
let create_int_val = "create_int_val"
let create_bool_val = "create_bool_val"
let create_string_val = "create_string_val"
let mult = "mult"
let divv = "divv"
let plus = "plus"
let minus = "minus"
let eq = "eq"
let req = "req"
let neq = "neq"
let rneq = "rneq"
let lt = "lt"
let lte = "lte"
let gt = "gt"
let gte = "gte"
let uplus = "uplus"
let uminus = "uminus"
let llor = "lor"
let lland = "land"
let create_empty_list_val = "create_empty_list_val"
let add_elem_to_list_val = "add_elem_to_list_val"
let list_head_getter = "list_head_getter"
let list_tail_getter = "list_tail_getter"
let list_length_getter = "list_length_getter"
let create_tuple = "create_tuple"
let tuple_getter = "tuple_getter"
let matching_failed = "matching_failed"
let print_int = "print_int"
let print_string = "print_string"
let create_closure = "create_closure"
let apply_closure = "apply_closure"
let call_closure = "call_closure"
let get_i1_val = "get_i1_val"

let runtime_functions =
  [ create_int_val, function_type ptr_type [| i32_type |]
  ; create_bool_val, function_type ptr_type [| bool_type |]
  ; create_string_val, function_type ptr_type [| ptr_type |]
  ; mult, function_type ptr_type [| ptr_type; ptr_type |]
  ; divv, function_type ptr_type [| ptr_type; ptr_type |]
  ; plus, function_type ptr_type [| ptr_type; ptr_type |]
  ; minus, function_type ptr_type [| ptr_type; ptr_type |]
  ; eq, function_type ptr_type [| ptr_type; ptr_type |]
  ; req, function_type ptr_type [| ptr_type; ptr_type |]
  ; neq, function_type ptr_type [| ptr_type; ptr_type |]
  ; rneq, function_type ptr_type [| ptr_type; ptr_type |]
  ; lt, function_type ptr_type [| ptr_type; ptr_type |]
  ; lte, function_type ptr_type [| ptr_type; ptr_type |]
  ; gt, function_type ptr_type [| ptr_type; ptr_type |]
  ; gte, function_type ptr_type [| ptr_type; ptr_type |]
  ; uplus, function_type ptr_type [| ptr_type |]
  ; uminus, function_type ptr_type [| ptr_type |]
  ; llor, function_type ptr_type [| ptr_type; ptr_type |]
  ; lland, function_type ptr_type [| ptr_type; ptr_type |]
  ; create_empty_list_val, function_type ptr_type [||]
  ; add_elem_to_list_val, function_type ptr_type [| ptr_type; ptr_type |]
  ; list_head_getter, function_type ptr_type [| ptr_type |]
  ; list_tail_getter, function_type ptr_type [| ptr_type |]
  ; list_length_getter, function_type ptr_type [| ptr_type |]
  ; create_tuple, var_arg_function_type ptr_type [| i32_type |]
  ; tuple_getter, function_type ptr_type [| ptr_type; ptr_type |]
  ; matching_failed, function_type void_type [||]
  ; print_int, function_type void_type [| ptr_type |]
  ; print_string, function_type void_type [| ptr_type |]
  ; create_closure, function_type ptr_type [| ptr_type; i32_type |]
  ; apply_closure, var_arg_function_type ptr_type [| ptr_type; i32_type |]
  ; call_closure, function_type ptr_type [| ptr_type |]
  ; get_i1_val, function_type bool_type [| ptr_type |]
  ]
;;

let name_to_runtime_func =
  let init = VarSMap.empty in
  let init = VarSMap.add "( * )" mult init in
  let init = VarSMap.add "( / )" divv init in
  let init = VarSMap.add "( + )" plus init in
  let init = VarSMap.add "( - )" minus init in
  let init = VarSMap.add "( = )" eq init in
  let init = VarSMap.add "( == )" req init in
  let init = VarSMap.add "( <> )" neq init in
  let init = VarSMap.add "( != )" rneq init in
  let init = VarSMap.add "( < )" lt init in
  let init = VarSMap.add "( <= )" lte init in
  let init = VarSMap.add "( > )" gt init in
  let init = VarSMap.add "( >= )" gte init in
  let init = VarSMap.add "( ~+ )" uplus init in
  let init = VarSMap.add "( ~- )" uminus init in
  let init = VarSMap.add "( || )" llor init in
  let init = VarSMap.add "( && )" lland init in
  let init = VarSMap.add "list_head_getter" list_head_getter init in
  let init = VarSMap.add "list_tail_getter" list_tail_getter init in
  let init = VarSMap.add "list_length_getter" list_length_getter init in
  let init = VarSMap.add "tuple_getter" tuple_getter init in
  let init = VarSMap.add "matching_failed" matching_failed init in
  let init = VarSMap.add "print_int" print_int init in
  let init = VarSMap.add "print_string" print_string init in
  init
;;

let find_env_name_value env s =
  match VarSMap.find_opt s env with
  | Some var -> var
  | None -> failwith "Error: Unbound value"
;;

let opt_find_ext_env_name_value ext_env runtime_name =
  VarSMap.find_opt runtime_name ext_env
;;

let opt_find_ext_name_type runtime_name =
  let res =
    List.find_opt (fun (cur_name, _) -> cur_name = runtime_name) runtime_functions
  in
  match res with
  | Some (_, typ) -> Some typ
  | None -> failwith "Internal Error: unbound runtime function name"
;;

let find_ext_func_info ext_env runtime_name =
  let llval = opt_find_ext_env_name_value ext_env runtime_name in
  let lltyp = opt_find_ext_name_type runtime_name in
  match llval, lltyp with
  | None, _ | _, None -> failwith "Internal Error: unbound runtime function name"
  | Some llval, Some lltyp -> llval, lltyp
;;

let find_var_name_value env ext_env s =
  match VarSMap.find_opt s env with
  | Some var -> var
  | None ->
    (match VarSMap.find_opt s name_to_runtime_func with
     | Some runtime_name ->
       (match opt_find_ext_env_name_value ext_env runtime_name with
        | Some func -> func
        | None -> failwith "Internal Error: Unbound runtime function name")
     | None -> failwith "Error: Unbound value")
;;

let bump ext_env id_val =
  match Llvm.classify_value id_val with
  | Llvm.ValueKind.Function ->
    let args_num = Array.length (params id_val) in
    let func, typ = find_ext_func_info ext_env create_closure in
    let closure =
      build_call
        typ
        func
        [| id_val; const_int i32_type args_num |]
        "empty_closure"
        builder
    in
    (match args_num with
     | 0 ->
       let func, typ = find_ext_func_info ext_env call_closure in
       build_call typ func [| closure |] "call_closure_res" builder
     | _ -> closure)
  | _ -> id_val
;;

let find_and_bump_function name env ext_env =
  let llval = find_var_name_value env ext_env name in
  bump ext_env llval
;;

let rec compile_immexpr env ext_env = function
  | Anf.ImmId s ->
    let id_val = find_and_bump_function s env ext_env in
    id_val
  | Anf.ImmInt n ->
    let func, typ = find_ext_func_info ext_env create_int_val in
    build_call typ func [| const_int i32_type n |] "int_var" builder
  | Anf.ImmString s ->
    let str_ptr = Llvm.build_global_stringptr s "tmp_str" builder in
    let func, typ = find_ext_func_info ext_env create_string_val in
    build_call typ func [| str_ptr |] "string_val" builder
  | Anf.ImmBool b ->
    let func, typ = find_ext_func_info ext_env create_bool_val in
    build_call typ func [| const_int bool_type (if b then 1 else 0) |] "bool_var" builder
  | Anf.ImmEmptyList ->
    let func, typ = find_ext_func_info ext_env create_empty_list_val in
    build_call typ func [||] "list_var" builder
  | Anf.ImmUnit ->
    let func, typ = find_ext_func_info ext_env create_bool_val in
    build_call typ func [| const_int bool_type 0 |] "bool_var" builder
  | Anf.ImmTuple im_lst ->
    let rev_llvalues =
      List.fold_left
        (fun curr_lst im ->
          let new_llvalue = compile_immexpr env ext_env im in
          new_llvalue :: curr_lst)
        []
        im_lst
    in
    let llvalues = List.rev rev_llvalues in
    let func, typ = find_ext_func_info ext_env create_tuple in
    let args_array =
      Array.of_list (const_int i32_type (List.length llvalues) :: llvalues)
    in
    build_call typ func args_array "tuple_var" builder
;;

let rec compile_cexpr env ext_env = function
  | Anf.CImmExpr immexpr -> compile_immexpr env ext_env immexpr
  | Anf.CApp (name, args) ->
    let closure = find_and_bump_function name env ext_env in
    let rev_llvalues =
      List.fold_left
        (fun curr_lst im ->
          let new_llvalue = compile_immexpr env ext_env im in
          new_llvalue :: curr_lst)
        []
        args
    in
    let llvalues = List.rev rev_llvalues in
    let func, typ = find_ext_func_info ext_env apply_closure in
    let last_val =
      List.fold_left
        (fun clos llval ->
          build_call
            typ
            func
            (Array.of_list [ clos; const_int i32_type 1; llval ])
            "apply_closure_res"
            builder)
        closure
        llvalues
    in
    last_val
    (* Можно передавать в closure сразу новые аргументы. Требует модификацию runtime `apply_closure` функции*)
    (* let args_array =
       Array.of_list (closure :: const_int i32_type (List.length llvalues) :: llvalues)
       in
       ( build_call
       apply_closure_func_type
       apply_closure_func
       args_array
       "apply_closure_res"
       builder
       ) *)
  | Anf.CIf (imm, aexpr1, aexpr2) ->
    let cond_val = compile_immexpr env ext_env imm in
    let func, typ = find_ext_func_info ext_env get_i1_val in
    let cond_i1_val = build_call typ func [| cond_val |] "cond_i1" builder in
    let func = Llvm.block_parent (Llvm.insertion_block builder) in
    let then_block = Llvm.append_block context "then" func in
    let else_block = Llvm.append_block context "else" func in
    let merge_block = Llvm.append_block context "merge" func in
    ignore (Llvm.build_cond_br cond_i1_val then_block else_block builder);
    Llvm.position_at_end then_block builder;
    let then_val = compile_aexpr env ext_env aexpr1 in
    ignore (Llvm.build_br merge_block builder);
    let then_block = insertion_block builder in
    Llvm.position_at_end else_block builder;
    let else_val = compile_aexpr env ext_env aexpr2 in
    ignore (Llvm.build_br merge_block builder);
    let else_block = insertion_block builder in
    Llvm.position_at_end merge_block builder;
    let phi =
      Llvm.build_phi [ then_val, then_block; else_val, else_block ] "if_res" builder
    in
    phi
  | Anf.CCons (im1, im2) ->
    let callh = compile_immexpr env ext_env im1 in
    let calltl = compile_immexpr env ext_env im2 in
    let func, typ = find_ext_func_info ext_env add_elem_to_list_val in
    build_call typ func [| callh; calltl |] "list_var" builder

and compile_aexpr env ext_env = function
  | Anf.ACExpr cexpr -> compile_cexpr env ext_env cexpr
  | Anf.ALet (name, cexpr, aexpr) ->
    let cexpr_val = compile_cexpr env ext_env cexpr in
    let env = VarSMap.add name cexpr_val env in
    compile_aexpr env ext_env aexpr
;;

let declare_runtime_functions =
  List.fold_left
    (fun ext_env (name, typ) ->
      let ext_func = Llvm.declare_function name typ the_module in
      VarSMap.add name ext_func ext_env)
    VarSMap.empty
    runtime_functions
;;

let declare_functions funcs env =
  List.fold_left
    (fun env (name, args, _) ->
      let func_type =
        match name with
        | "main" -> function_type i32_type (Array.make (List.length args) ptr_type)
        | _ -> function_type ptr_type (Array.make (List.length args) ptr_type)
      in
      let func = declare_function name func_type the_module in
      VarSMap.add name func env)
    env
    funcs
;;

let compile_functions funcs env ext_env =
  List.iter
    (fun (name, args, aexpr) ->
      let func = find_env_name_value env name in
      let entry = append_block context "entry" func in
      Llvm.position_at_end entry builder;
      let env =
        List.fold_left2
          (fun env arg param -> VarSMap.add arg param env)
          env
          args
          (Array.to_list (Llvm.params func))
      in
      let ret = compile_aexpr env ext_env aexpr in
      match name with
      | "main" -> ignore (Llvm.build_ret (const_int i32_type 0) builder)
      | _ -> ignore (Llvm.build_ret ret builder))
    funcs
;;

let compile_program program =
  let ext_env = declare_runtime_functions in
  let env =
    List.fold_left
      (fun env (_, funcs) -> declare_functions funcs env)
      VarSMap.empty
      program
  in
  List.iter (fun (_, funcs) -> compile_functions funcs env ext_env) program
;;

let generate_ir (program : Anf.program) =
  compile_program program;
  string_of_llmodule the_module
;;

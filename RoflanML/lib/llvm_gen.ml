open Llvm
open Anf_ast

let symbol_table : (string, llvalue) Hashtbl.t = Hashtbl.create 10
let function_type_table : (string, lltype) Hashtbl.t = Hashtbl.create 10

let map_name_to_runtime name =
  match name with
  | "+" -> "+"
  | "-" -> "-"
  | "*" -> "*"
  | "=" -> "="
  | _ -> name
;;

let find_symbol name =
  match Hashtbl.find_opt symbol_table (map_name_to_runtime name) with
  | Some v -> v
  | None -> failwith (Printf.sprintf "Undefined symbol: %s" name)
;;

let find_function_type name =
  match Hashtbl.find_opt function_type_table (map_name_to_runtime name) with
  | Some t -> t
  | None -> failwith (Printf.sprintf "Undefined function type: %s" name)
;;

let register_symbol name value = Hashtbl.add symbol_table (map_name_to_runtime name) value

let register_function_type name ftype =
  Hashtbl.add function_type_table (map_name_to_runtime name) ftype
;;

(* Контекст LLVM и базовые объекты *)
let llvm_context = global_context ()
let llvm_module = create_module llvm_context "Roflan"
let llvm_builder = builder llvm_context
let int64_type = i64_type llvm_context
let bool_type = i1_type llvm_context
let int32_type = i32_type llvm_context
let value_pointer_type = pointer_type llvm_context

let rec generate_immexpr (expr : immexpr) : llvalue =
  match expr with
  | ImmConst c ->
    (match c with
     | CInt i ->
       build_call
         (find_function_type "create_int")
         (find_symbol "create_int")
         [| const_int int64_type i |]
         "boxed_int"
         llvm_builder
     | CBool b ->
       build_call
         (find_function_type "create_bool")
         (find_symbol "create_bool")
         [| const_int bool_type (if b then 1 else 0) |]
         "boxed_bool"
         llvm_builder
     | CUnit ->
       build_call
         (find_function_type "create_unit")
         (find_symbol "create_unit")
         [||]
         "boxed_unit"
         llvm_builder)
  | ImmVar name ->
    let value = find_symbol name in
    (match classify_value value with
     | ValueKind.Function ->
       let arity = Array.length (params value) in
       generate_closure value arity
     | _ -> value)
  | ImmTuple (e1, e2, rest) ->
    let elements = List.map generate_immexpr (e1 :: e2 :: rest) in
    build_call
      (find_function_type "create_tuple")
      (find_symbol "create_tuple")
      (Array.of_list (const_int int32_type (List.length elements) :: elements))
      "tuple"
      llvm_builder
  | ImmList lst ->
    let empty_list =
      build_call
        (find_function_type "create_empty_list")
        (find_symbol "create_empty_list")
        [||]
        "empty_list"
        llvm_builder
    in
    List.fold_right
      (fun elem acc ->
        build_call
          (find_function_type "list_cons")
          (find_symbol "list_cons")
          [| generate_immexpr elem; acc |]
          "list_cons"
          llvm_builder)
      lst
      empty_list

and generate_cexpr (expr : cexpr) : llvalue =
  match expr with
  | CImm ie -> generate_immexpr ie
  | CBranch (cond_expr, then_expr, else_expr) ->
    let cond_val = generate_immexpr cond_expr in
    let bool_val =
      build_call
        (find_function_type "get_bool")
        (find_symbol "get_bool")
        [| cond_val |]
        "cond_bool"
        llvm_builder
    in
    let current_func = block_parent (insertion_block llvm_builder) in
    let then_block = append_block llvm_context "then" current_func in
    let else_block = append_block llvm_context "else" current_func in
    let merge_block = append_block llvm_context "merge" current_func in
    ignore (build_cond_br bool_val then_block else_block llvm_builder);
    position_at_end then_block llvm_builder;
    let then_val = generate_aexpr then_expr in
    ignore (build_br merge_block llvm_builder);
    let then_bb = insertion_block llvm_builder in
    position_at_end else_block llvm_builder;
    let else_val = generate_aexpr else_expr in
    ignore (build_br merge_block llvm_builder);
    let else_bb = insertion_block llvm_builder in
    position_at_end merge_block llvm_builder;
    build_phi [ then_val, then_bb; else_val, else_bb ] "branch_result" llvm_builder
  | CApp (fn_expr, arg_exprs) ->
    let func_val = generate_immexpr fn_expr in
    let maybe_closure =
      match classify_value func_val with
      | ValueKind.Function -> generate_closure func_val (Array.length (params func_val))
      | _ -> func_val
    in
    List.fold_left
      (fun acc arg ->
        build_call
          (find_function_type "apply")
          (find_symbol "apply")
          [| acc; generate_immexpr arg |]
          "apply_result"
          llvm_builder)
      maybe_closure
      arg_exprs

and generate_aexpr (expr : aexpr) : llvalue =
  match expr with
  | ALetIn (name, ce, body) ->
    let value = generate_cexpr ce in
    register_symbol name value;
    generate_aexpr body
  | ACExpr ce -> generate_cexpr ce

and generate_closure func arity =
  build_call
    (find_function_type "create_closure")
    (find_symbol "create_closure")
    [| build_bitcast func (pointer_type llvm_context) "func_ptr_cast" llvm_builder
     ; const_int int64_type arity
    |]
    "closure"
    llvm_builder
;;

let declare_function (name, args, _) =
  let func_type =
    function_type value_pointer_type (Array.make (List.length args) value_pointer_type)
  in
  let func = Llvm.declare_function name func_type llvm_module in
  register_symbol name func;
  register_function_type name func_type;
  func
;;

let generate_function (_, name, args, body) =
  let func = find_symbol name in
  let entry = append_block llvm_context "entry" func in
  position_at_end entry llvm_builder;
  List.iteri
    (fun i (arg_name, _) ->
      let param = (params func).(i) in
      set_value_name arg_name param;
      register_symbol arg_name param)
    args;
  ignore (build_ret (generate_aexpr body) llvm_builder)
;;

let generate_main program =
  let main_type = function_type int32_type [||] in
  let main_func = Llvm.declare_function "main" main_type llvm_module in
  let entry = append_block llvm_context "entry" main_func in
  position_at_end entry llvm_builder;
  (match
     List.find_opt
       (function
         | ADLet (NonRec, name, [], _) when name = "()" -> true
         | _ -> false)
       program
   with
   | Some (ADLet (_, "()", [], body)) -> ignore (generate_aexpr body)
   | _ ->
     List.iter
       (function
         | ADLet (NonRec, name, [], body) when name <> "()" ->
           let global_var =
             define_global name (const_null value_pointer_type) llvm_module
           in
           let compiled_val = generate_aexpr body in
           ignore (build_store compiled_val global_var llvm_builder)
         | _ -> ())
       program);
  ignore (build_ret (const_int int32_type 0) llvm_builder)
;;

(* Регистрация заглушек runtime-функций *)
let register_dummy_runtime () =
  let declare_dummy name llvm_t =
    let func = Llvm.declare_function name llvm_t llvm_module in
    register_symbol name func;
    register_function_type name llvm_t;
    func
  in
  ignore
    (declare_dummy
       "="
       (Llvm.function_type
          value_pointer_type
          [| value_pointer_type; value_pointer_type |]));
  ignore
    (declare_dummy
       "+"
       (Llvm.function_type
          value_pointer_type
          [| value_pointer_type; value_pointer_type |]));
  ignore
    (declare_dummy
       "-"
       (Llvm.function_type
          value_pointer_type
          [| value_pointer_type; value_pointer_type |]));
  ignore
    (declare_dummy
       "*"
       (Llvm.function_type
          value_pointer_type
          [| value_pointer_type; value_pointer_type |]));
  ignore
    (declare_dummy "create_int" (Llvm.function_type value_pointer_type [| int64_type |]));
  ignore
    (declare_dummy "create_bool" (Llvm.function_type value_pointer_type [| bool_type |]));
  ignore (declare_dummy "create_unit" (Llvm.function_type value_pointer_type [||]));
  ignore (declare_dummy "create_empty_list" (Llvm.function_type value_pointer_type [||]));
  ignore
    (declare_dummy
       "list_cons"
       (Llvm.function_type
          value_pointer_type
          [| value_pointer_type; value_pointer_type |]));
  ignore
    (declare_dummy
       "apply"
       (Llvm.function_type
          value_pointer_type
          [| value_pointer_type; value_pointer_type |]));
  ignore
    (declare_dummy "get_bool" (Llvm.function_type bool_type [| value_pointer_type |]));
  ignore
    (declare_dummy
       "create_closure"
       (Llvm.function_type value_pointer_type [| pointer_type llvm_context; int64_type |]))
;;

let compile_program (program : aprogram) : Llvm.llmodule =
  register_dummy_runtime ();
  List.iter
    (function
      | ADMutualLet funcs ->
        List.iter
          (fun (name, args, body) ->
            let _ = declare_function (name, args, body) in
            ())
          funcs;
        List.iter
          (fun (name, args, body) ->
            ignore (generate_function (Ast.Rec, name, args, body)))
          funcs
      | _ -> ())
    program;
  List.iter
    (function
      | ADLet (rec_flag, name, args, body) when args <> [] ->
        let _ = declare_function (name, args, body) in
        ignore (generate_function (rec_flag, name, args, body))
      | _ -> ())
    program;
  generate_main program;
  llvm_module
;;

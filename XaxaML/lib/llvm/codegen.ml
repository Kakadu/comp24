(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open! Llvm
open! Anf
open! Common_llvm
open! Stdlib_llvm
open! Symbol_map

let rec compile_aexp : aexp -> llvalue = function
  | Ae_int i ->
    let create_int_func = lookup_symbol "create_int" in
    let create_int_type = lookup_function_type "create_int" in
    build_call create_int_type create_int_func [| const_int i64_t i |] "boxed_int" builder
  | Ae_bool b ->
    let create_bool_func = lookup_symbol "create_bool" in
    let create_bool_type = lookup_function_type "create_bool" in
    build_call
      create_bool_type
      create_bool_func
      [| const_int bool_t (if b then 1 else 0) |]
      "boxed_bool"
      builder
  | Ae_unit ->
    let create_unit_func = lookup_symbol "create_unit" in
    let create_unit_type = lookup_function_type "create_unit" in
    build_call create_unit_type create_unit_func [||] "boxed_unit" builder
  | Ae_val name ->
    let value = lookup_symbol name in
    let maybe_closure =
      match Llvm.classify_value value with
      | Llvm.ValueKind.Function ->
        let arity = Array.length (params value) in
        compile_closure value arity
      | _ -> value
    in
    maybe_closure
  | Ae_empty_list ->
    let empty_list_func = lookup_symbol "create_empty_list" in
    let ft = lookup_function_type "create_empty_list" in
    build_call ft empty_list_func [||] "empty_list" builder
  | Ae_tuple lst ->
    let compiled_args = List.map compile_aexp lst in
    let args_count = List.length compiled_args in
    let create_tuple = lookup_symbol "create_tuple" in
    let tuple_type = lookup_function_type "create_tuple" in
    build_call
      tuple_type
      create_tuple
      (Array.of_list (const_int i32_t args_count :: compiled_args))
      "tuple"
      builder

and compile_app name args =
  (* Получаем значение функции *)
  let func_val = lookup_symbol name in
  (* Создаем замыкание для функции, если это функция *)
  let maybe_closure =
    match Llvm.classify_value func_val with
    | Llvm.ValueKind.Function ->
      let arity = Array.length (params func_val) in
      compile_closure func_val arity
    | _ -> func_val
  in
  (* Применяем аргументы через apply *)
  let apply_func = lookup_symbol "apply" in
  let apply_type = lookup_function_type "apply" in
  List.fold_left
    (fun acc_closure arg ->
      let compiled_arg = compile_aexp arg in
      build_call
        apply_type
        apply_func
        [| acc_closure; compiled_arg |]
        "apply_result"
        builder)
    maybe_closure
    args

and compile_ite cond t f =
  let cond_val = compile_aexp cond in
  (* Получаем функцию get_bool для извлечения булева значения *)
  let get_bool_func = lookup_symbol "get_bool" in
  let get_bool_type = lookup_function_type "get_bool" in
  let bool_val =
    build_call get_bool_type get_bool_func [| cond_val |] "cond_bool" builder
  in
  (* Создаем базовые блоки для IF-THEN-ELSE *)
  let current_func = block_parent (insertion_block builder) in
  let then_block = append_block ctx "then" current_func in
  let else_block = append_block ctx "else" current_func in
  let merge_block = append_block ctx "merge" current_func in
  (* Создаем условный переход *)
  ignore (build_cond_br bool_val then_block else_block builder);
  (* Компилируем THEN блок *)
  position_at_end then_block builder;
  let then_val = compile_exp t in
  ignore (build_br merge_block builder);
  let then_bb = insertion_block builder in
  (* Компилируем ELSE блок *)
  position_at_end else_block builder;
  let else_val = compile_exp f in
  ignore (build_br merge_block builder);
  let else_bb = insertion_block builder in
  (* Создаем PHI-узел для объединения результатов *)
  position_at_end merge_block builder;
  let phi = build_phi [ then_val, then_bb; else_val, else_bb ] "ite_result" builder in
  phi

and compile_cons_list car cdr =
  let compiled_car = compile_aexp car in
  let compiled_cdr = compile_aexp cdr in
  let cons_func = lookup_symbol "list_cons" in
  let cons_type = lookup_function_type "list_cons" in
  build_call cons_type cons_func [| compiled_car; compiled_cdr |] "list_cons" builder

and compile_cexp = function
  | Ce_atom aexp -> compile_aexp aexp
  | Ce_app (f, args) -> compile_app f args
  | Ce_ite (cond, t, f) -> compile_ite cond t f
  | Ce_cons_list (car, cdr) -> compile_cons_list car cdr

and compile_exp = function
  | E_complex cexp -> compile_cexp cexp
  | E_let_in (name, cexp, e) ->
    let val_result = compile_cexp cexp in
    add_symbol name val_result;
    compile_exp e

and compile_closure func arity =
  (* Создаем замыкание для функции *)
  let create_closure_func = lookup_symbol "create_closure" in
  let create_closure_type = lookup_function_type "create_closure" in
  (* Создаем указатель на функцию *)
  let function_ptr = build_bitcast func (pointer_type ctx) "func_ptr_cast" builder in
  (* Создаем замыкание и возвращаем его *)
  build_call
    create_closure_type
    create_closure_func
    [| function_ptr; const_int i64_t arity |]
    "closure"
    builder
;;

let declare_function (name, args, _) =
  let func_type = function_type value_ptr_t (Array.make (List.length args) value_ptr_t) in
  let func = declare_function name func_type the_module in
  add_symbol name func;
  add_function_type name func_type;
  func
;;

let compile_function (name, args, exp) =
  (* Получаем функцию, объявленную ранее *)
  let func = lookup_symbol name in
  (* Компилируем тело функции *)
  let entry = append_block ctx "entry" func in
  position_at_end entry builder;
  (* Получаем параметры функции *)
  let llvm_params = params func in
  (* Связываем параметры с именами аргументов *)
  List.iteri
    (fun i arg_name ->
      let param = Array.get llvm_params i in
      set_value_name arg_name param;
      add_symbol arg_name param)
    args;
  (* Компилируем тело функции *)
  let body_return = compile_exp exp in
  (* Добавляем возврат - это должно быть последней инструкцией в блоке *)
  ignore (build_ret body_return builder)
;;

let compile_value name value =
  (* Глобальная переменная должна быть уже создана, получаем её *)
  let global_var = lookup_symbol name in
  (* Компилируем значение в контексте main *)
  let compiled_val = compile_exp value in
  (* Сохраняем скомпилированное значение в глобальной переменной *)
  ignore (build_store compiled_val global_var builder);
  (* Добавляем символ, указывающий на скомпилированное значение *)
  add_symbol name compiled_val;
  compiled_val
;;

let declare_global_value name value =
  (* Создаем глобальную переменную для хранения значения *)
  let global_var = define_global name (const_null value_ptr_t) the_module in
  add_symbol name global_var;
  (* Возвращаем созданную глобальную переменную *)
  global_var
;;

let compile_main program =
  (* Создаем точку входа программы *)
  let main_type = function_type i32_t [||] in
  let main_func = Llvm.declare_function "main" main_type the_module in
  let entry = append_block ctx "entry" main_func in
  position_at_end entry builder;
  (* Инициализируем значения *)
  List.iter
    (function
      | Value (name, value) -> ignore (compile_value name value)
      | _ -> ())
    program;
  (* Возвращаем успешный код завершения *)
  ignore (build_ret (const_int i32_t 0) builder)
;;

let compile_program ?(verbose = false) (program : anf_program) =
  (* Объявляем внешние функции и стандартные операции *)
  List.iter declare_foreign_function foreign_funcs;
  (* Компилируем определения функций и значений из программы *)
  List.iter
    (function
      | Non_rec func ->
        let _ = declare_function func in
        compile_function func
      | Rec funcs ->
        List.iter (fun f -> ignore (declare_function f)) funcs;
        List.iter compile_function funcs
      | Value (name, value) -> ignore (declare_global_value name value))
    program;
  (* Компилируем точку входа *)
  compile_main program;
  (* Вывод LLVM IR кода *)
  print_module "out.ll" the_module;
  if verbose then dump_module the_module
;;

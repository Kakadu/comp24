open Llvm
open Llast
open Ast
open Base

let context = global_context ()
let the_module = create_module context "MEML"
let builder = builder context
let i64_type = i64_type context
let map_empty = Map.empty (module String)

let const_codegen = function
  | CInt i -> const_int i64_type i
  | CBool b ->
    let int_b = if b then 1 else 0 in
    const_int i64_type int_b
  | _ -> failwith "Unsupported constant"
;;

let op_codegen code_lle1 code_lle2 builder = function
  | Add -> build_add code_lle1 code_lle2 "add_result" builder
  | Sub -> build_sub code_lle1 code_lle2 "sub_result" builder
  | Mul -> build_mul code_lle1 code_lle2 "mul_result" builder
  | Div -> build_sdiv code_lle1 code_lle2 "div_result" builder
  | And -> build_and code_lle1 code_lle2 "and_result" builder
  | Or -> build_or code_lle1 code_lle2 "or_result" builder
  | Eq ->
    build_zext
      (build_icmp Icmp.Eq code_lle1 code_lle2 "eq_result" builder)
      i64_type
      "eq_result"
      builder
  | Neq ->
    build_zext
      (build_icmp Icmp.Ne code_lle1 code_lle2 "neq_result" builder)
      i64_type
      "neq_result"
      builder
  | Less ->
    build_zext
      (build_icmp Icmp.Slt code_lle1 code_lle2 "less_result" builder)
      i64_type
      "less_result"
      builder
  | Gre ->
    build_zext
      (build_icmp Icmp.Sgt code_lle1 code_lle2 "gre_result" builder)
      i64_type
      "gre_result"
      builder
  | Leq ->
    build_zext
      (build_icmp Icmp.Sle code_lle1 code_lle2 "leq_result" builder)
      i64_type
      "leq_result"
      builder
  | Greq ->
    build_zext
      (build_icmp Icmp.Sge code_lle1 code_lle2 "greq_result" builder)
      i64_type
      "greq_result"
      builder
;;

let find global_vars v =
  match Map.find global_vars v with
  | Some value ->
    let f_type, f_func, args = value in
    if Array.length args = 0
    then build_call f_type f_func [||] "call_result" builder
    else f_func
  | None -> failwith ("Undefined variable or function: " ^ v)
;;

let rec llexpression_codegen local_vars global_vars the_function = function
  | LLConst c -> const_codegen c
  | LLEbinOp (op, lle1, lle2) ->
    let code_lle1 = llexpression_codegen local_vars global_vars the_function lle1 in
    let code_lle2 = llexpression_codegen local_vars global_vars the_function lle2 in
    op_codegen code_lle1 code_lle2 builder op
  | LLVar v ->
    (* Ищем переменную сначала в local_vars, затем в global_vars *)
    (match List.Assoc.find local_vars ~equal:String.( = ) v with
     | Some value -> value
     | None -> find global_vars v)
  | LLApp (lle1, lle2) ->
    let rec helper acc = function
      | LLApp (lle1, lle2) -> helper (lle2 :: acc) lle1
      | a -> llexpression_codegen local_vars global_vars the_function a, acc
    in
    let f_func, args = helper [] (LLApp (lle1, lle2)) in
    let llvm_args =
      List.fold
        ~init:[]
        ~f:(fun acc llarg ->
          llexpression_codegen local_vars global_vars the_function llarg :: acc)
        args
    in
    let arg_types = Array.init (List.length llvm_args) ~f:(fun _ -> i64_type) in
    let f_type = function_type i64_type arg_types in
    build_call f_type f_func (List.to_array llvm_args) "call_result" builder
  | LLIfElse (cond_expr, then_expr, else_expr) ->
    (* Генерируем код условия *)
    let cond_value = llexpression_codegen local_vars global_vars the_function cond_expr in
    (* Преобразуем i64 в i1 (0 -> false, все остальное -> true) *)
    let zero = const_int i64_type 0 in
    let cond_bool = build_icmp Icmp.Ne cond_value zero "ifcond" builder in
    (* Создаём базовые блоки *)
    let then_bb = append_block context "then" the_function in
    let else_bb = append_block context "else" the_function in
    let merge_bb = append_block context "merge" the_function in
    (* Добавляем условный переход *)
    ignore (build_cond_br cond_bool then_bb else_bb builder);
    (* === Генерируем код для then-блока === *)
    position_at_end then_bb builder;
    let then_value = llexpression_codegen local_vars global_vars the_function then_expr in
    ignore (build_br merge_bb builder);
    let then_bb = insertion_block builder in
    (* Фиксируем конец then-блока *)
    (* === Генерируем код для else-блока === *)
    position_at_end else_bb builder;
    let else_value = llexpression_codegen local_vars global_vars the_function else_expr in
    ignore (build_br merge_bb builder);
    let else_bb = insertion_block builder in
    (* Фиксируем конец else-блока *)
    (* === Генерируем merge-блок с phi-инструкцией === *)
    position_at_end merge_bb builder;
    let phi_node =
      build_phi [ then_value, then_bb; else_value, else_bb ] "iftmp" builder
    in
    phi_node (* Возвращаем результат if-выражения *)
  | _ -> failwith "Unsupported expression"
;;

let pattern_codegen = function
  | PVar (v, _) -> v
  | _ -> failwith "Unsupported pattern"
;;

let codegen_llbindings global_vars = function
  | LLLet (r, name, args, lle) ->
    (* Создаём функцию f: define i64 @f() *)
    let arg_types = Array.init (List.length args) ~f:(fun _ -> i64_type) in
    let f_type = function_type i64_type arg_types in
    let f_func = define_function name f_type the_module in
    let new_map = Map.set global_vars ~key:name ~data:(f_type, f_func, arg_types) in
    (* Именуем аргументы и создаём таблицу символов *)
    let local_vars =
      List.mapi
        ~f:(fun i arg ->
          let param = param f_func i in
          set_value_name (pattern_codegen arg) param;
          pattern_codegen arg, param)
        args
    in
    (* Получаем начальный блок функции и позиционируемся в нём *)
    let bb = entry_block f_func in
    position_at_end bb builder;
    (* Генерируем код для выражения lle, передавая таблицу символов *)
    let result =
      if Poly.( = ) r Notrec
      then llexpression_codegen local_vars global_vars f_func lle
      else llexpression_codegen local_vars new_map f_func lle
    in
    (* Добавляем возврат результата *)
    ignore (build_ret result builder);
    new_map
  | _ -> failwith "Unsupported binding"
;;

let codegen llstatments =
  let _ =
    List.fold
      ~init:map_empty
      ~f:(fun let_map llbindings ->
        let new_map = codegen_llbindings let_map llbindings in
        new_map)
      llstatments
  in
  Stdlib.print_endline (string_of_llmodule the_module)
;;

(* let generate_llvm_ir () =
  (* Создаём контекст и модуль *)
  let context = global_context () in
  let the_module = create_module context "MEML" in
  let builder = builder context in
  let i64_type = i64_type context in

  (* Создаём функцию f: define i64 @f(i64 %a) *)
  let f_type = function_type i64_type [| i64_type |] in  (* Функция принимает один аргумент типа i64 *)
  let f_func = define_function "f" f_type the_module in

  (* Получаем аргумент функции f *)
  let arg = param f_func 0 in  (* Получаем первый аргумент (индекс 0) *)
  set_value_name "a" arg;      (* Даём аргументу имя "a" *)

  (* Получаем начальный блок функции f и позиционируемся в нём *)
  let f_bb = entry_block f_func in
  position_at_end f_bb builder;

  (* Добавляем вычисление: a + 4 *)
  let four = const_int i64_type 4 in
  let result = build_add arg four "result" builder in

  (* Добавляем возврат результата *)
  ignore (build_ret result builder);

  (* Создаём функцию main: define i64 @main() *)
  let main_type = function_type i64_type [||] in  (* main не принимает аргументов *)
  let main_func = define_function "main" main_type the_module in

  (* Получаем начальный блок функции main и позиционируемся в нём *)
  let main_bb = entry_block main_func in
  position_at_end main_bb builder;

  (* Вызываем функцию f с аргументом 5 *)
  let five = const_int i64_type 5 in
  let call_result = build_call f_type f_func [| five |] "call_result" builder in

  (* Возвращаем результат вызова f *)
  ignore (build_ret call_result builder);

  (* Выводим LLVM IR *)
  print_endline (string_of_llmodule the_module);
;; *)

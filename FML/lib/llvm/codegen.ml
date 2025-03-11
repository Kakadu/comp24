(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Llvm
open Anf_ast

let sym_to_value : (string, llvalue) Hashtbl.t = Hashtbl.create 1
let sym_to_type : (string, lltype) Hashtbl.t = Hashtbl.create 1
let lookup_name name = Hashtbl.find_opt sym_to_value name
let lookup_type name = Hashtbl.find_opt sym_to_type name
let add_sym name value = Hashtbl.add sym_to_value name value
let add_type name ty = Hashtbl.add sym_to_type name ty

let id_to_runtime_name = function
  | "( + )" -> "add"
  | "( - )" -> "sub"
  | "( * )" -> "mul"
  | "( / )" -> "divd"
  | "( = )" -> "eq"
  | "( != )" -> "neq"
  | "( < )" -> "less"
  | "( <= )" -> "leq"
  | "( > )" -> "gre"
  | "( >= )" -> "geq"
  | "( && )" -> "and"
  | "( || )" -> "or"
  | other -> other
;;

let ctx = global_context ()
let builder = builder ctx
let module_ = create_module ctx "FML"
let target_triple = Llvm_target.Target.default_triple ()
let () = Llvm.set_target_triple target_triple module_
let i64_t = i64_type ctx

let compile_binop op x y =
  match op with
  | "( + )" -> build_add x y "add" builder
  | "( - )" -> build_sub x y "sub" builder
  | "( * )" -> build_mul x y "mul" builder
  | "( / )" -> build_sdiv x y "div" builder
  | _ -> failwith ("Invalid operator: " ^ op)
;;

let compile_immexpr = function
  | ImmInt n -> const_int i64_t n
  | ImmBool b -> const_int i64_t (Bool.to_int b)
  | ImmUnit -> const_int i64_t 0
  | ImmIdentifier name ->
    let name = id_to_runtime_name name in
    (match lookup_function name module_ with
     | Some f ->
       let fun_ptr = build_ptrtoint f i64_t "" builder in
       build_call
         (function_type i64_t [| i64_t; i64_t |])
         (Option.get @@ lookup_function "create_closure" module_)
         [| fun_ptr; const_int i64_t (Array.length (params f)); const_int i64_t 0 |]
         "empty_closure"
         builder
     | None ->
       (match lookup_global name module_ with
        | Some g -> g
        | None ->
          (match lookup_name name with
           | Some v -> v
           | None -> failwith ("Unknown variable: " ^ name))))
  | _ -> failwith "Not implemented"
;;

let rec compile_cexpr = function
  | CImmExpr expr -> compile_immexpr expr
  | CEApply (name, args) ->
    let compiled_args = List.map compile_immexpr args in
    (match lookup_function name module_ with
     | Some f when Array.length (params f) = List.length args ->
       build_call (type_of f) f (Array.of_list compiled_args) name builder
     | Some f ->
       let fun_ptr = build_ptrtoint f i64_t "" builder in
       let cl =
         build_call
           (function_type i64_t [| i64_t; i64_t |])
           (Option.get @@ lookup_function "create_closure" module_)
           [| fun_ptr; const_int i64_t (Array.length (params f)); const_int i64_t 0 |]
           "closure"
           builder
       in
       build_call
         (var_arg_function_type i64_t [| i64_t; i64_t |])
         (Option.get (lookup_function "apply_args_to_closure" module_))
         (Array.of_list
            ([ cl; const_int i64_t (List.length compiled_args) ] @ compiled_args))
         "applied_closure"
         builder
     | None -> failwith "Not a function")
  | CEIf (cond, then_e, else_e) ->
    let cond_v =
      build_icmp Icmp.Ne (compile_immexpr cond) (const_int i64_t 0) "cond_v" builder
    in
    let entry_block = insertion_block builder in
    let parent = block_parent entry_block in
    let then_block = append_block ctx "then" parent in
    position_at_end then_block builder;
    let then_ = compile_aexpr then_e in
    let new_then_block = insertion_block builder in
    let else_block = append_block ctx "else" parent in
    position_at_end else_block builder;
    let else_ = compile_aexpr else_e in
    let new_else_block = insertion_block builder in
    let merge_bb = append_block ctx "merge" parent in
    position_at_end merge_bb builder;
    let phi = build_phi [ then_, new_then_block; else_, new_else_block ] "phi" builder in
    position_at_end entry_block builder;
    let (_ : llvalue) = build_cond_br cond_v then_block else_block builder in
    position_at_end new_then_block builder;
    let (_ : llvalue) = build_br merge_bb builder in
    position_at_end new_else_block builder;
    let (_ : llvalue) = build_br merge_bb builder in
    position_at_end merge_bb builder;
    phi
  | _ -> failwith "Not impemented"

and compile_aexpr = function
  | ACExpr expr -> compile_cexpr expr
  | ALetIn (name, ce, ae) ->
    let v = compile_cexpr ce in
    add_sym name v;
    compile_aexpr ae
;;

let declare_func name args =
  let arg_types = Array.make (List.length args) i64_t in
  let func_type = function_type i64_t arg_types in
  declare_function name func_type module_
;;

let compile_anf_binding (ALet (name, args, body)) =
  let func = declare_func name args in
  let bb = append_block ctx "entry" func in
  position_at_end bb builder;

  List.iteri (fun i arg_name ->
    let arg_value = param func i in
    set_value_name arg_name arg_value;
    add_sym arg_name arg_value;
  ) args;

  let body_val = compile_aexpr body in
  let _ = build_ret body_val builder in
  func
;;


let compile_anf_decl = function
  | ADNoRec bindings ->
    List.iter (fun binding -> ignore (compile_anf_binding binding)) bindings
  | ADREC bindings ->
    List.iter (fun (ALet (name, args, _)) -> ignore (declare_func name args)) bindings;
    List.iter (fun binding -> ignore (compile_anf_binding binding)) bindings
;;



let init_runtime () =
  let runtime_ =
    [ "create_closure", function_type i64_t [| i64_t; i64_t; i64_t |]
    ; "apply_args_to_closure", var_arg_function_type i64_t [| i64_t; i64_t; i64_t |]
    ; "print_int", function_type i64_t [| i64_t |]
    ; "print_bool", function_type i64_t [| i64_t |]
    ; "add", function_type i64_t [| i64_t; i64_t |]
    ; "sub", function_type i64_t [| i64_t; i64_t |]
    ; "mul", function_type i64_t [| i64_t; i64_t |]
    ; "div", function_type i64_t [| i64_t; i64_t |]
    ; "leq", function_type i64_t [| i64_t; i64_t |]
    ; "less", function_type i64_t [| i64_t; i64_t |]
    ; "geq", function_type i64_t [| i64_t; i64_t |]
    ; "gre", function_type i64_t [| i64_t; i64_t |]
    ; "eq", function_type i64_t [| i64_t; i64_t |]
    ; "neq", function_type i64_t [| i64_t; i64_t |]
    ; "and", function_type i64_t [| i64_t; i64_t |]
    ; "or", function_type i64_t [| i64_t; i64_t |]
    ; "fail_match", function_type i64_t [| i64_t |]
    ]
  in
  List.iter (fun (name, ty) -> ignore (declare_function name ty module_)) runtime_
;;


let create_main program =
  let main_type = function_type i64_t [||] in
  let main = declare_function "main" main_type module_ in
  let bb = append_block ctx "entry" main in
  position_at_end bb builder;

  init_runtime ();

  List.iter (fun decl -> ignore (compile_anf_decl decl)) program;

  let _ = build_ret (const_int i64_t 0) builder in
  main
;;

let compile_program program filename =
  let _ = create_main program in
  print_module filename module_
;;
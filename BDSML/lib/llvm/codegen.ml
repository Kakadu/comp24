open Llvm
open Llvm_init
open Middleend.Anf_ast
open Parser.Ast

let rec compile_aexp = function
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
    let types = List.map compile_aexp elems in
    const_struct context (Array.of_list types)
  | AExp_construct ("::", _) as l ->
    let l' = build_list l in
    let types = List.map compile_aexp l' in
    const_struct context (Array.of_list types)
  | AExp_construct ("[]", _) -> const_struct context (Array.of_list [])
  | AExp_construct ("None", _) -> build_none
  | AExp_construct ("Some", x) ->
    (match x with
     | None -> failwith "Impossible"
     | Some x -> build_some (compile_aexp x))
  | AExp_construct (_, _) -> failwith "Not implemented"
  | AExp_ident id -> build_ident id
;;

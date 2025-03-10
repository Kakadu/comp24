open Llvm
open Llvm_init
open Middleend.Anf_ast
open Parser.Ast

let rec compile_aexpr = function
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
  | AExp_construct ("None", _) -> build_none
  | AExp_construct ("Some", x) ->
    (match x with
     | None -> failwith "Impossible"
     | Some x -> build_some (compile_aexpr x))
  | AExp_construct (_, _) -> failwith "Not implemented"
  | AExp_ident id -> build_ident id
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

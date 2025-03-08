(** Copyright 2024-2025, Perevalov Efim, Ermolovich Anna *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anfast
open Llvm
open Result
open Ast

let context = global_context ()
let the_module = create_module context "MEML"
let builder = builder context
let i64_type = i64_type context
let void_type = void_type context
let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 20
let ( let* ) = bind

let const_codegen = function
  | AInt x -> ok (const_int i64_type x)
  | ABool b -> ok (const_int i64_type (Base.Bool.to_int b))
  | _ -> error "List not Implemented"
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

let name_op = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | And -> "And"
  | Or -> "Or"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Less -> "Less"
  | Gre -> "Gre"
  | Leq -> "Leq"
  | Greq -> "Greq"
;;

let rec llexpression_codegen = function
  | AConst c -> const_codegen c
  | AVar "()" -> ok (const_pointer_null void_type)
  | AVar id ->
    (match Hashtbl.find_opt named_values id with
     | Some v -> ok (build_load i64_type v id builder)
     | None ->
       (match lookup_function id the_module with
        | Some v ->
          ok
            (build_call
               (function_type i64_type [| i64_type; i64_type |])
               (Option.get (lookup_function "addInClosure" the_module))
               [| build_pointercast v i64_type "ptr_to_int" builder
                ; params v |> Base.Array.length |> const_int i64_type
               |]
               "Closure"
               builder)
        | None -> error "Unknown variable"))
  | ABinOp (op, l, r) ->
    (match lookup_function (name_op op) the_module with
     | Some _ -> llexpression_codegen @@ AApp (AApp (AVar (name_op op), l), r)
     | None ->
       let* l' = llexpression_codegen l in
       let* r' = llexpression_codegen r in
       let res = op_codegen l' r' builder op in
       ok (build_zext res i64_type "to_int" builder))
  | AApp (func, argument) ->
    let* calee = llexpression_codegen func in
    let* arg = llexpression_codegen argument in
    ok
      (build_call
         (function_type i64_type [| i64_type; i64_type |])
         (Option.get (lookup_function "appClosure" the_module))
         [| calee; arg |]
         "AppClosure"
         builder)
  | AIfElse (cond_expr, then_expr, else_expr) ->
    let start_bb = insertion_block builder in
    let the_function = block_parent start_bb in
    let* cond_value = llexpression_codegen cond_expr in
    let zero = const_int i64_type 0 in
    let cond_bool = build_icmp Icmp.Ne cond_value zero "if" builder in
    let then_bb = append_block context "then" the_function in
    let else_bb = append_block context "else" the_function in
    let merge_bb = append_block context "merge" the_function in
    ignore (build_cond_br cond_bool then_bb else_bb builder);
    position_at_end then_bb builder;
    let* then_value = llexpression_codegen then_expr in
    ignore (build_br merge_bb builder);
    let then_bb = insertion_block builder in
    position_at_end else_bb builder;
    let* else_value = llexpression_codegen else_expr in
    ignore (build_br merge_bb builder);
    let else_bb = insertion_block builder in
    position_at_end merge_bb builder;
    let phi_node =
      build_phi [ then_value, then_bb; else_value, else_bb ] "iftmp" builder
    in
    ok phi_node
  | AVars (l, r) ->
    let _ = llexpression_codegen l in
    llexpression_codegen r
  | _ -> error "Not implemented"
;;

let llbindings_codegen = function
  | ALet (_, id, args, body) ->
    Hashtbl.clear named_values;
    let ints = Array.make (List.length args) i64_type in
    let ftype = function_type i64_type ints in
    let* func =
      match lookup_function id the_module with
      | Some _ -> error "Function already exists"
      | None -> ok @@ declare_function id ftype the_module
    in
    let* names =
      let rec check acc = function
        | [] -> ok (List.rev acc)
        | "_" :: xs -> check ("wild" :: acc) xs
        | id :: xs -> check (id :: acc) xs
      in
      check [] args
    in
    Array.iteri
      (fun i a ->
        let name = List.nth names i in
        set_value_name name a)
      (params func);
    let bb = append_block context "entry" func in
    position_at_end bb builder;
    Array.iteri
      (fun i a ->
        let name = List.nth names i in
        let alloca = build_alloca i64_type name builder in
        let (_ : Llvm.llvalue) = build_store a alloca builder in
        Hashtbl.add named_values name alloca)
      (params func);
    let* ret_val = llexpression_codegen body in
    let _ =
      if id = "main"
      then build_ret (const_int i64_type 0) builder
      else build_ret ret_val builder
    in
    ok func
  | _ -> error "Not implemented"
;;

let codegen llstatments =
  let runtime =
    [ declare_function
        "addInClosure"
        (function_type i64_type [| i64_type; i64_type |])
        the_module
    ; declare_function
        "appClosure"
        (function_type i64_type [| i64_type; i64_type |])
        the_module
    ; declare_function "print_int" (function_type i64_type [| i64_type |]) the_module
    ]
  in
  let* result =
    List.fold_left
      (fun acc bexpr ->
        let* acc = acc in
        let* res = llbindings_codegen bexpr in
        ok (res :: acc))
      (ok runtime)
      llstatments
  in
  ok (List.rev result)
;;

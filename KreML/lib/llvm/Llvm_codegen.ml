(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm
open Flambda
open Ast
open Anf
open Utils
open Utils.Counter
open Llvm_utils

let context = global_context ()
let mdl = create_module context "main"
let builder = Llvm.builder context
let named_values : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 16
let functions_types : (string, Llvm.lltype) Hashtbl.t = Hashtbl.create 16
let curr_decl_name = ref ""
let int_type = Llvm.i64_type context
let iconst = Llvm.const_int int_type
let ptr_type = pointer_type context

let lookup_fun_exn name =
  match Llvm.lookup_function name mdl with
  | None -> Utils.internalfail @@ Format.sprintf "Unknown function %s" name
  | Some f -> f
;;

let lookup_global_exn name =
  match Llvm.lookup_global name mdl with
  | Some v -> v
  | None -> Utils.internalfail @@ Format.sprintf "Unknown global %s" name
;;

let alloc_tuple_typ = Llvm.function_type ptr_type [| int_type |]

let alloc_tuple name size =
  let* size = size in
  if size = 0
  then lookup_global_exn "nil" |> return
  else (
    let size_const = iconst size in
    let f = lookup_fun_exn Runtime.alloc_tuple in
    build_call alloc_tuple_typ f [| size_const |] name builder |> return)
;;

let fun_with_env_typ = Llvm.function_type int_type [| ptr_type; int_type |]
let call_closure_typ = Llvm.function_type int_type [| ptr_type; ptr_type; int_type |]

(* closure, args, args_count*)
let alloc_closure_typ =
  Llvm.function_type ptr_type [| ptr_type; ptr_type; int_type; int_type |]
;;

(* fptr, env, arity, envsize *)

let list_cons_typ = Llvm.function_type ptr_type [| int_type; ptr_type |]

let alloc_closure f env arity env_size =
  let* f = f in
  let* fresh = fresh_name "closure_temp" in
  let callee = lookup_fun_exn Runtime.alloc_closure in
  build_call
    alloc_closure_typ
    callee
    [| f; env; iconst arity; iconst env_size |]
    fresh
    builder
  |> return
;;

let call_closure c args =
  let* c = c in
  let* c =
    if type_of c <> ptr_type
    then
      let* fresh_name = fresh_name "cast" in
      build_inttoptr c ptr_type fresh_name builder |> return
    else return c
  in
  let* fresh = fresh_name "call_closure" in
  let count = List.length args in
  let* tupled_args = fresh_name "tupled_args" in
  let* args_tuple = alloc_tuple tupled_args (return count) in
  let* _ =
    List.fold_left
      (fun idx arg ->
        let* idx = idx in
        let* fresh = fresh_name "elemptr" in
        let elemptr = build_gep int_type args_tuple [| iconst idx |] fresh builder in
        let _ = build_store arg elemptr builder in
        return (idx + 1))
      (return 0)
      args
  in
  let callee = lookup_fun_exn "call_closure" in
  build_call call_closure_typ callee [| c; args_tuple; iconst count |] fresh builder
  |> return
;;

let print_int_typ = function_type int_type [| int_type |]

let add_builtin_functions () =
  let _ = declare_function Runtime.alloc_tuple alloc_tuple_typ mdl in
  let _ = declare_function Runtime.alloc_closure alloc_closure_typ mdl in
  let _ = declare_function Runtime.call_closure call_closure_typ mdl in
  let _ = declare_function Cstdlib.print_int print_int_typ mdl in
  Hashtbl.add functions_types Cstdlib.print_int print_int_typ;
  let _ = declare_function Runtime.list_cons list_cons_typ mdl in
  let pmtype = function_type (void_type context) [| int_type |] in
  let _ = declare_function Runtime.partial_match pmtype mdl in
  ()
;;

let set_globals () =
  let _ = define_global "nil" (const_null ptr_type) mdl in
  let _ = define_global "unit" (iconst 0) mdl in
  ()
;;

let codegen_const = function
  | Const_int i -> iconst i
  | Const_bool c ->
    let i1_typ = i1_type context in
    if c then const_int i1_typ 1 else const_int i1_typ 0
  | Const_nil -> lookup_global_exn "nil"
  | Const_unit -> lookup_global_exn "unit"
;;

let codegen_binop name x y =
  let xtyp = type_of x in
  let ytyp = type_of y in
  let x, y =
    if xtyp <> ytyp
    then (
      let xsize = integer_bitwidth xtyp in
      let ysize = integer_bitwidth ytyp in
      if xsize < ysize
      then x, build_trunc y xtyp "" builder
      else build_trunc x ytyp "" builder, y)
    else x, y
  in
  function
  | Eq -> build_icmp Icmp.Eq x y name builder
  | Neq -> build_icmp Icmp.Ne x y name builder
  | Gt -> build_icmp Icmp.Sgt x y name builder
  | Geq -> build_icmp Icmp.Sge x y name builder
  | Lt -> build_icmp Icmp.Slt x y name builder
  | Leq -> build_icmp Icmp.Sle x y name builder
  | Plus | Or -> build_add x y name builder
  | Minus -> build_sub x y name builder
  | Mul | And -> build_mul x y name builder
  | Div -> build_sdiv x y name builder
;;

let rec codegen_flambda = function
  | Fl_const c -> codegen_const c |> return
  | Fl_var v ->
    (match Hashtbl.find_opt named_values v with
     | Some v -> return v
     | None -> internalfail @@ Format.sprintf "Unbound variable %s" v)
  | Fl_tuple elems ->
    let* elems = codegen_list (return elems) in
    let* tuple = alloc_tuple !curr_decl_name (List.length elems |> return) in
    let* _ =
      List.fold_left
        (fun idx value ->
          let* idx = idx in
          let* fresh = fresh_name "elemptr" in
          let elemptr = build_gep int_type tuple [| iconst idx |] fresh builder in
          let _ = build_store value elemptr builder in
          return (idx + 1))
        (return 0)
        elems
    in
    return tuple
  | Fl_binop (op, x, y) ->
    let* x = codegen_flambda x in
    let* y = codegen_flambda y in
    let* x, y =
      match type_of x, type_of y with
      | xtype, ytype when xtype = ytype -> return (x, y)
      | xtype, _ when xtype = ptr_type ->
        let* cast = fresh_name "cast" in
        let y = build_inttoptr y ptr_type cast builder in
        return (x, y)
      | _, ytype when ytype = ptr_type ->
        let* cast = fresh_name "cast" in
        let x = build_inttoptr x ptr_type cast builder in
        return (x, y)
      | _ -> unreachable ()
    in
    return @@ codegen_binop !curr_decl_name x y op
  | Fl_cons (x, xs) ->
    let* x' = codegen_flambda x in
    let* xs' = codegen_flambda xs in
    let* xs' =
      if type_of xs' <> ptr_type
      then
        let* cast = fresh_name "cast" in
        build_inttoptr xs' ptr_type cast builder |> return
      else return xs'
    in
    let cons = lookup_fun_exn Runtime.list_cons in
    build_call list_cons_typ cons [| x'; xs' |] !curr_decl_name builder |> return
  | Fl_getfield (idx, obj) ->
    let* obj = codegen_flambda obj in
    let* obj =
      if type_of obj <> ptr_type
      then
        let* fresh = fresh_name "cast" in
        build_inttoptr obj ptr_type fresh builder |> return
      else return obj
    in
    let* fresh = fresh_name "fieldptr" in
    let elemptr = build_gep int_type obj [| iconst idx |] fresh builder in
    let* fresh = fresh_name "load" in
    build_load int_type elemptr fresh builder |> return
  | Fl_app (Fl_var "partial_match", [ arg ]) ->
    (* special case to insert unreacheable *)
    let* arg = codegen_flambda arg in
    let callee = lookup_fun_exn Runtime.partial_match in
    let callee_typ = function_type (void_type context) [| int_type |] in
    let _ = build_call callee_typ callee [| arg |] "" builder in
    build_unreachable builder |> return
  | Fl_app (Fl_closure { name; env_size; arity; _ }, (([ _ ] | []) as args))
    when env_size = 0 && arity <> 0 ->
    (* if arity 0 it is calculated const, we should evail it and apply arg to its result *)
    let* args = codegen_list (return args) in
    let callee = lookup_fun_exn name in
    let callee_typ = Hashtbl.find functions_types name in
    let* args =
      match args with
      | [ a ] when type_of a = ptr_type ->
        let* fresh = fresh_name "cast" in
        let a = build_ptrtoint a int_type fresh builder in
        return [| a |]
      | [ a ] -> return [| a |]
      | _ -> return [||]
    in
    build_call callee_typ callee args !curr_decl_name builder |> return
  | Fl_app (f, args) ->
    let* args = codegen_list (return args) in
    let* f' = codegen_flambda f in
    call_closure (return f') args
  | Fl_ite (c, t, e) ->
    let* c = codegen_flambda c in
    let* fresh_reg = fresh_name "ifcmp" in
    let is_zero = codegen_binop fresh_reg c (const_int (i1_type context) 0) Eq in
    let start_bb = insertion_block builder in
    let f = block_parent start_bb in
    let* fresh_then = fresh_name "then" in
    let then_bb = append_block context fresh_then f in
    position_at_end then_bb builder;
    let* t = codegen_flambda t in
    let then_bb_after_gen = insertion_block builder in
    let* fresh_else = fresh_name "else" in
    let else_bb = append_block context fresh_else f in
    position_at_end else_bb builder;
    let* e = codegen_flambda e in
    let else_bb_after_gen = insertion_block builder in
    let* fresh_merge = fresh_name "merge" in
    let t, e =
      (* types may differ if either t or e is a function call, which
         return value is int, and the other one is a pointer. we cast int to pointer *)
      let ttype, etype = type_of t, type_of e in
      match ttype, etype with
      | _ when ttype = etype || etype = void_type context -> t, e
      | ttype, _ when ttype = ptr_type ->
        position_at_end else_bb_after_gen builder;
        t, build_inttoptr e ptr_type "" builder
      | _, etype when etype = ptr_type ->
        position_at_end then_bb_after_gen builder;
        build_inttoptr t ptr_type "" builder, e
      | _ -> unreachable ()
    in
    let merge_bb = append_block context fresh_merge f in
    position_at_end merge_bb builder;
    let* fresh_phi = fresh_name "phi" in
    let incoming =
      [ t, then_bb_after_gen; e, else_bb_after_gen ]
      |> List.filter (fun (_, b) -> block_terminates_with_unreachable b |> not)
    in
    let phi = build_phi incoming fresh_phi builder in
    position_at_end start_bb builder;
    let _ = build_cond_br is_zero else_bb then_bb builder in
    position_at_end then_bb_after_gen builder;
    if block_terminates_with_unreachable then_bb_after_gen |> not
    then ignore @@ build_br merge_bb builder;
    position_at_end else_bb_after_gen builder;
    if block_terminates_with_unreachable else_bb_after_gen |> not
    then ignore @@ build_br merge_bb builder;
    position_at_end merge_bb builder;
    return phi
  | Fl_let (None, v, scope) ->
    curr_decl_name := "";
    let* _ = codegen_flambda v in
    codegen_flambda scope
  | Fl_let (Some name, v, scope) ->
    curr_decl_name := name;
    let* v = codegen_flambda v in
    set_value_name name v;
    Hashtbl.add named_values name v;
    codegen_flambda scope
  | Fl_closure { name; arrange = []; env_size = 0; arity = 0 } ->
    let callee = lookup_fun_exn name in
    let callee_typ = Hashtbl.find functions_types name in
    let* fresh = fresh_name "call" in
    build_call callee_typ callee [||] fresh builder |> return
  | Fl_closure { name; arrange; env_size; arity } ->
    let callee = lookup_fun_exn name in
    let* name = fresh_name "tupled_env" in
    let* env = alloc_tuple name (return env_size) in
    let* arrange_env =
      List.fold_right
        (fun (idx, value) acc ->
          let* acc = acc in
          let idx = iconst idx in
          let* value' = codegen_flambda value in
          (idx, value') :: acc |> return)
        arrange
        (return [])
    in
    let* () =
      List.fold_left
        (fun acc (idx, value) ->
          let* () = acc in
          let* fresh = fresh_name "envptr" in
          let elemptr = build_gep int_type env [| idx |] fresh builder in
          let _ = build_store value elemptr builder in
          return ())
        (return ())
        arrange_env
    in
    alloc_closure (return callee) env arity env_size

and codegen_list list =
  let* list = list in
  List.fold_right
    (fun e acc ->
      let* acc = acc in
      let* e' = codegen_flambda e in
      e' :: acc |> return)
    list
    (return [])
;;

let codegen_fun_body f decl =
  let* f = f in
  let* body =
    match f with
    | Fun_with_env { arg = arg_name; env_vars; body; _ } ->
      let env = param decl 0 in
      set_value_name "env" env;
      let arg = param decl 1 in
      set_value_name arg_name arg;
      let entry_bb = append_block context "entry" decl in
      position_at_end entry_bb builder;
      (* unpack environment *)
      let* _ =
        List.fold_left
          (fun idx name ->
            let* idx = idx in
            let* fresh = fresh_name "envelemptr" in
            let elemptr = build_gep int_type env [| iconst idx |] fresh builder in
            let local = build_load int_type elemptr name builder in
            Hashtbl.add named_values name local;
            return (idx + 1))
          (return 0)
          env_vars
      in
      (* name arg *)
      set_value_name arg_name arg;
      Hashtbl.add named_values arg_name arg;
      let* body = codegen_flambda body in
      let body_bb = insertion_block builder in
      position_at_end body_bb builder;
      return body
    | Fun_without_env (None, body) ->
      let entry = append_block context "entry" decl in
      position_at_end entry builder;
      let* body = codegen_flambda body in
      return body
    | Fun_without_env (Some param_name, body) ->
      let entry = append_block context "entry" decl in
      position_at_end entry builder;
      let param = param decl 0 in
      set_value_name param_name param;
      Hashtbl.add named_values param_name param;
      let* body = codegen_flambda body in
      let body_bb = insertion_block builder in
      position_at_end body_bb builder;
      return body
  in
  let* () =
    if type_of body = ptr_type
    then
      let* fresh = fresh_name "cast" in
      let cast = build_ptrtoint body int_type fresh builder in
      let _ = build_ret cast builder in
      return ()
    else if type_of body = i1_type context
    then (
      let _ = build_ret (build_sext body int_type "" builder) builder in
      return ())
    else (
      let _ = build_ret body builder in
      return ())
  in
  (* some merge blocks may have not termination instruction
     since ret is inserted only in the last block *)
  let _ =
    fold_right_blocks
      (fun b next ->
        let terminator = block_terminator b in
        match terminator with
        | None when block_terminates_with_unreachable b |> not ->
          position_at_end b builder;
          let _ = build_br next builder in
          b
        | Some _ | None -> b)
      decl
      (entry_block decl)
  in
  return ()
;;

let codegen_program program =
  add_builtin_functions ();
  set_globals ();
  let declarations =
    List.map
      (fun (name, f) ->
        match f with
        | Fun_with_env _ ->
          let f = declare_function name fun_with_env_typ mdl in
          Hashtbl.add functions_types name fun_with_env_typ;
          f
        | Fun_without_env (None, _) ->
          let fnty = function_type int_type [||] in
          let f = declare_function name fnty mdl in
          Hashtbl.add functions_types name fnty;
          f
        | Fun_without_env (Some _, _) ->
          let fnty = function_type int_type [| int_type |] in
          let f = declare_function name fnty mdl in
          Hashtbl.add functions_types name fnty;
          f)
      program
  in
  let state =
    List.fold_left2
      (fun acc (_, f) decl ->
        let* () = acc in
        let* () = codegen_fun_body (return f) decl in
        return ())
      (return ())
      program
      declarations
  in
  let _, _ = run state 0 in
  match Llvm_analysis.verify_module mdl with
  | None -> ()
  | Some s ->
    let open Stdlib.Format in
    fprintf std_formatter "%s" s;
    ()
;;

let dump program =
  codegen_program program;
  dump_module mdl
;;

let get_module program =
  codegen_program program;
  mdl
;;

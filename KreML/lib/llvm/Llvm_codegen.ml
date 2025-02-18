(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm
open Flambda
open Ast
open Anf
open Llvm_utils

type codegen_state =
  { counter : int
  ; curr_decl_name : string
  ; named_values : (Ast.ident, llvalue, Base.String.comparator_witness) Base.Map.t
  ; fun_types : (Ast.ident, lltype, Base.String.comparator_witness) Base.Map.t
  }

module CodegenMonad = Utils.State (struct
    type t = codegen_state
  end)

open CodegenMonad

let fresh_name base =
  let* ({ counter; _ } as state) = get in
  let res = Format.sprintf "%s_%i" base counter in
  let* _ = put { state with counter = counter + 1 } in
  return res
;;

let put_curr_decl_name name =
  let* state = get in
  put { state with curr_decl_name = name }
;;

let get_curr_decl_name =
  let* { curr_decl_name; _ } = get in
  return curr_decl_name
;;

let put_named_value name value =
  let open CodegenMonad in
  let* ({ named_values; _ } as state) = get in
  put { state with named_values = Base.Map.set named_values ~key:name ~data:value }
;;

let put_fun_type name typ =
  let open CodegenMonad in
  let* ({ fun_types; _ } as state) = get in
  put { state with fun_types = Base.Map.set fun_types ~key:name ~data:typ }
;;

let lookup_named_value name =
  let open CodegenMonad in
  let* { named_values; _ } = get in
  Base.Map.find_exn named_values name |> return
;;

let lookup_fun_type name =
  let open CodegenMonad in
  let* name = name in
  let* { fun_types; _ } = get in
  Base.Map.find_exn fun_types name |> return
;;

let context = global_context ()
let mdl = create_module context "main"
let builder = Llvm.builder context
let int_type = Llvm.i64_type context
let iconst = Llvm.const_int int_type
let ptr_type = pointer_type context
let max_args_count = 8

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

open CodegenMonad

let alloc_tuple name size =
  let* size = size in
  if size = 0
  then lookup_global_exn "nil" |> return
  else (
    let size_const = iconst size in
    let f = lookup_fun_exn Runtime.alloc_tuple in
    build_call alloc_tuple_typ f [| size_const |] name builder |> return)
;;

let call_closure_typ = Llvm.function_type int_type [| ptr_type; ptr_type; int_type |]

(* closure, args, args_count*)
let alloc_closure_typ =
  Llvm.function_type ptr_type [| ptr_type; ptr_type; int_type; int_type |]
;;

(* fptr, env, arity, envsize *)

let list_cons_typ = Llvm.function_type ptr_type [| int_type; ptr_type |]

let alloc_closure f env arity fv_count =
  let* f = f in
  let* fresh = fresh_name "closure_temp" in
  let callee = lookup_fun_exn Runtime.alloc_closure in
  build_call
    alloc_closure_typ
    callee
    [| f; env; iconst arity; iconst fv_count |]
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
  let* _ = put_fun_type Cstdlib.print_int print_int_typ in
  let _ = declare_function Runtime.list_cons list_cons_typ mdl in
  let pmtype = function_type (void_type context) [| int_type |] in
  let _ = declare_function Runtime.partial_match pmtype mdl in
  return ()
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
  | Fl_var v -> lookup_named_value v
  | Fl_tuple elems ->
    let* elems = codegen_list (return elems) in
    let* name = get_curr_decl_name in
    let* tuple = alloc_tuple name (List.length elems |> return) in
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
      | _ -> Utils.unreachable ()
    in
    let* name = get_curr_decl_name in
    return @@ codegen_binop name x y op
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
    let* name = get_curr_decl_name in
    build_call list_cons_typ cons [| x'; xs' |] name builder |> return
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
  | Fl_app (Fl_closure { name; env_size; arity; _ }, args)
    when env_size = 0 && arity = List.length args && arity <= max_args_count ->
    (* if arity 0 it is calculated const, we should evail it and apply arg to its result *)
    let* args = codegen_list (return args) in
    let callee = lookup_fun_exn name in
    let* callee_typ = lookup_fun_type (return name) in
    let args =
      List.map
        (fun arg ->
          if type_of arg = ptr_type then build_ptrtoint arg int_type "" builder else arg)
        args
    in
    let* name = get_curr_decl_name in
    build_call callee_typ callee (Array.of_list args) name builder |> return
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
      | _ -> Utils.unreachable ()
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
    then (
      let _ = build_br merge_bb builder in
      ());
    position_at_end else_bb_after_gen builder;
    if block_terminates_with_unreachable else_bb_after_gen |> not
    then (
      let _ = build_br merge_bb builder in
      ());
    position_at_end merge_bb builder;
    return phi
  | Fl_let (None, v, scope) ->
    let* _ = put_curr_decl_name "" in
    let* _ = codegen_flambda v in
    codegen_flambda scope
  | Fl_let (Some name, v, scope) ->
    let* _ = put_curr_decl_name name in
    let* v = codegen_flambda v in
    set_value_name name v;
    let* _ = put_named_value name v in
    codegen_flambda scope
  | Fl_closure { name; arrange = []; env_size = 0; arity = 0 } ->
    let callee = lookup_fun_exn name in
    let* callee_typ = lookup_fun_type (return name) in
    let* fresh = fresh_name "call" in
    build_call callee_typ callee [||] fresh builder |> return
  | Fl_closure { name; arrange; env_size = fv_count; arity } ->
    let callee = lookup_fun_exn name in
    let* name = fresh_name "tupled_env" in
    let* env = alloc_tuple name (return (fv_count + arity)) in
    (* let* arrange_env =
       List.fold_right
       (fun (idx, value) acc ->
       let* acc = acc in
       let idx = iconst idx in
       let* value' = codegen_flambda value in
       (idx, value') :: acc |> return)
       arrange
       (return [])
       in *)
    let* () =
      List.fold_left
        (fun acc (idx, value) ->
          let* () = acc in
          let* value = codegen_flambda value in
          let* fresh = fresh_name "envptr" in
          let elemptr = build_gep int_type env [| iconst idx |] fresh builder in
          let _ = build_store value elemptr builder in
          return ())
        (return ())
        arrange
    in
    alloc_closure (return callee) env arity fv_count

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
    | Fun_without_env { param_names; arity; body } when arity <= max_args_count ->
      let llparams = params decl |> Array.to_list in
      let* () =
        List.fold_left2
          (fun acc p llp ->
            let* () = acc in
            set_value_name p llp;
            let* _ = put_named_value p llp in
            return ())
          (return ())
          param_names
          llparams
      in
      let entry = append_block context "entry" decl in
      position_at_end entry builder;
      let* body = codegen_flambda body in
      return body
    | Fun_without_env { param_names; body; _ } | Fun_with_env { param_names; body; _ } ->
      let env = param decl 0 in
      set_value_name "env" env;
      let llparams = Base.List.drop (params decl |> Array.to_list) 1 in
      let params, env_params = Base.List.split_n param_names (List.length llparams) in
      (* name args and put them in named values*)
      let* () =
        List.fold_left2
          (fun acc p llp ->
            let* () = acc in
            set_value_name p llp;
            let* _ = put_named_value p llp in
            return ())
          (return ())
          params
          llparams
      in
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
            let* _ = put_named_value name local in
            return (idx + 1))
          (return 0)
          env_params
      in
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
  let* _ = add_builtin_functions () in
  set_globals ();
  let* declarations =
    List.fold_left
      (fun acc (name, f) ->
        let* acc = acc in
        let f, ty =
          let params =
            match f with
            | Fun_without_env { arity; _ } when arity <= max_args_count ->
              List.init arity (fun _ -> int_type)
            | Fun_without_env { arity; _ } | Fun_with_env { arity; _ } ->
              let all_params = [ ptr_type ] @ List.init arity (fun _ -> int_type) in
              if List.length all_params > max_args_count
              then Utils.ListUtils.take max_args_count all_params
              else all_params
          in
          let fnty = function_type int_type (Array.of_list params) in
          let f = declare_function name fnty mdl in
          f, fnty
        in
        let* _ = put_fun_type name ty in
        return @@ acc @ [ f ])
      (return [])
      program
  in
  let* _ =
    List.fold_left2
      (fun acc (_, f) decl ->
        let* () = acc in
        let* () = codegen_fun_body (return f) decl in
        return ())
      (return ())
      program
      declarations
  in
  match Llvm_analysis.verify_module mdl with
  | None -> return ()
  | Some s ->
    let open Stdlib.Format in
    fprintf std_formatter "%s" s;
    return ()
;;

let codegen_program p =
  let _, _ =
    run
      (codegen_program p)
      { counter = 0
      ; curr_decl_name = ""
      ; named_values = Base.Map.empty (module Base.String)
      ; fun_types = Base.Map.empty (module Base.String)
      }
  in
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

open Llvm
open Flambda
open Ast
open Anf
open Utils
open Utils.Counter

(* let fresh_name base =
  get >>= fun ({ counter ; _} as state) ->
  let res = Format.sprintf "%s_%i" base counter in
  put {state with counter = counter + 1} >>= fun _ -> return res

let lookup_arity name =
  let* name in
  let* {arities; _} = get in
  match Base.Map.find arities name with
  | Some a -> return a
  | None -> Utils.internalfail @@ Format.sprintf "arity of function %s is unknown" name *)

let context = global_context ()
let mdl = create_module context "main"
let builder = Llvm.builder context
let named_values : (string, Llvm.llvalue) Hashtbl.t = Hashtbl.create 16
let int_type = Llvm.i64_type context
let iconst = Llvm.const_int int_type
let ptr_type = pointer_type context

(* let cast2ptr value =
   let* value = value in
   let* fresh = fresh_name "temp" in
   build_pointercast value int_type fresh builder |> return
   ;; *)

let lookup_fun_exn name =
  match Llvm.lookup_function name mdl with
  | None -> Utils.internalfail @@ Format.sprintf "Unknown function %s" name
  | Some f -> f
;;

let alloc_tuple_typ = Llvm.function_type ptr_type [| int_type |]

let alloc_tuple name size =
  let* size = size in
  let size_const = iconst size in
  let f = lookup_fun_exn Runtime.alloc_tuple in
  build_call alloc_tuple_typ f [| size_const |] name builder |> return
;;

(* let alloc_closure_typ = Llvm.function_type int_type [| int_type; int_type |] *)

let fun_with_env_typ = Llvm.function_type int_type [| ptr_type; int_type |]
let fun_without_env_typ = Llvm.function_type int_type [| int_type |]
let call_closure_typ = Llvm.function_type int_type [| ptr_type; ptr_type; int_type |]

(* closure, args, args_count*)
let alloc_closure_typ = Llvm.function_type ptr_type [| ptr_type; ptr_type; int_type |]
(* fptr, env, arity *)

let alloc_closure f env arity =
  let* f = f in
  let* fresh = fresh_name "closure_temp" in
  let callee = lookup_fun_exn Runtime.alloc_closure in
  build_call alloc_closure_typ callee [| f; env; iconst arity |] fresh builder |> return
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

let print_int_typ = function_type (void_type context) [| int_type |]

let add_runtime_functions () =
  let _ = declare_function Runtime.alloc_tuple alloc_tuple_typ mdl in
  let _ = declare_function Runtime.alloc_closure alloc_tuple_typ mdl in
  let _ = declare_function Runtime.call_closure call_closure_typ mdl in
  let _ = declare_function Runtime.print_int print_int_typ mdl in
  ()
;;

let codegen_const = function
  | Const_int i -> iconst i
  | Const_bool c -> if c then iconst 1 else iconst 0
  | _ -> Utils.internalfail "todo"
;;

let codegen_binop name x y = function
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
     | Some v -> v |> return
     | None ->
       Utils.internalfail @@ Format.sprintf "variable was not found in context %s" v)
  | Fl_tuple elems ->
    let* elems = codegen_list (return elems) in
    let* name = fresh_name "tuple" in
    let* tuple = alloc_tuple name (List.length elems |> return) in
    let* _ =
      List.fold_left
        (fun idx value ->
          let* idx = idx in
          let* fresh = fresh_name "temp_tuple" in
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
    let* fresh = fresh_name "temp" in
    return @@ codegen_binop fresh x y op
  | Fl_cons _ -> Utils.internalfail "todo"
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
  | Fl_app (Fl_var stdlib_fun, args) when Runtime.is_stdlib_fun stdlib_fun ->
    let callee = lookup_fun_exn stdlib_fun in
    let* args = codegen_list (return args) in
    let fnty = function_type (void_type context) [| int_type |] in
    build_call fnty callee (Array.of_list args) "" builder |> return
  | Fl_app (f, args) ->
    let* f' = codegen_flambda f in
    let* args = codegen_list (return args) in
    (match f, args with
     | Fl_closure { name; env_size; _ }, [ arg ] when env_size = 0 ->
       let callee = lookup_fun_exn name in
       let* fresh = fresh_name "direct_call" in
       build_call fun_without_env_typ callee [| arg |] fresh builder |> return
     | _ -> call_closure (return f') args)
  | Fl_ite (c, t, e) ->
    let* c = codegen_flambda c in
    let* freshreg = fresh_name "ifcmp" in
    let is_zero = build_icmp Icmp.Eq (const_int (i1_type context) 0) c freshreg builder in
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
    let merge_bb = append_block context fresh_merge f in
    position_at_end merge_bb builder;
    let* fresh_phi = fresh_name "phi" in
    let phi =
      build_phi [ t, then_bb_after_gen; e, else_bb_after_gen ] fresh_phi builder
    in
    position_at_end then_bb_after_gen builder;
    let _ = build_br merge_bb builder in
    position_at_end start_bb builder;
    let _ = build_cond_br is_zero else_bb then_bb builder in
    position_at_end else_bb builder;
    let _ = build_br merge_bb builder in
    position_at_end merge_bb builder;
    return phi
  | Fl_let (id, (Fl_app (Fl_var print, _) as call_print), scope)
    when print = Runtime.print_int ->
    let* call = codegen_flambda call_print in
    Hashtbl.add named_values id call;
    codegen_flambda scope
  | Fl_let (name, Fl_binop (op, x, y), scope) ->
    let* x = codegen_flambda x in
    let* y = codegen_flambda y in
    let op = codegen_binop name x y op in
    Hashtbl.add named_values name op;
    codegen_flambda scope
  | Fl_let (name, v, scope) ->
    let* v = codegen_flambda v in
    set_value_name name v;
    Hashtbl.add named_values name v;
    codegen_flambda scope
  | Fl_closure { name; arrange; env_size } ->
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
    alloc_closure (return callee) env (env_size + 1)

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

let codegen_fun name f =
  let* f = f in
  let* body =
    match f with
    | Fun_with_env { arg = arg_name; captured_args; body; _ } ->
      let f = declare_function name fun_with_env_typ mdl in
      let env = param f 0 in
      set_value_name "env" env;
      let arg = param f 1 in
      set_value_name arg_name arg;
      let entry_bb = append_block context "entry" f in
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
          captured_args
      in
      (* name arg *)
      set_value_name arg_name arg;
      Hashtbl.add named_values arg_name arg;
      let* body = codegen_flambda body in
      let body_bb = insertion_block builder in
      position_at_end body_bb builder;
      return body
    | Fun_without_env (None, body) ->
      let f =
        declare_function name (function_type int_type [| |]) mdl
      in
      let entry = append_block context "entry" f in
      position_at_end entry builder;
      let* body = codegen_flambda body in
      return body
    | Fun_without_env (Some param_name, body) ->
      let f = declare_function name (function_type int_type [| int_type |]) mdl in
      let entry = append_block context "entry" f in
      position_at_end entry builder;
      let param = param f 0 in
      set_value_name param_name param;
      Hashtbl.add named_values param_name param;
      let* body = codegen_flambda body in
      let body_bb = insertion_block builder in
      position_at_end body_bb builder;
      return body
  in
  if type_of body <> int_type
  then
    let* fresh = fresh_name "cast" in
    let cast = build_ptrtoint body int_type fresh builder in
    let _ = build_ret cast builder in
    return ()
  else (
    let _ = build_ret body builder in
    return ())
;;

let codegen_program program =
  add_runtime_functions ();
  let state =
    List.fold_left
      (fun acc (id, f) ->
        let* () = acc in
        let* () = codegen_fun id (return f) in
        return ())
      (return ())
      program
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

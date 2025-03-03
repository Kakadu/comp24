open Llvm
open Ast
open Anf_ast
open Base

let context = global_context ()
let the_module = create_module context "Roflan"
let i64 = i64_type context
let env : (id, llvalue) Hashtbl.t = Hashtbl.create (module String)
let global_inits : (string * (unit -> llvalue)) list ref = ref []

let codegen_const = function
  | CInt i -> const_int i64 i
  | CBool b -> const_int i64 (if b then 1 else 0)
  | CUnit -> const_int i64 0
;;

let codegen_immexpr (imm : immexpr) : llvalue =
  match imm with
  | ImmConst c -> codegen_const c
  | ImmVar id ->
    (match Hashtbl.find env id with
     | Some v -> v
     | None -> failwith ("Undefined variable: " ^ id))
  | ImmTuple _ -> failwith "Tuple generation not implemented"
  | ImmList _ -> failwith "List generation not implemented"
;;

let codegen_cexpr (ce : cexpr) : llvalue =
  match ce with
  | CImm imm -> codegen_immexpr imm
  | CBranch _ ->
    failwith "Branch generation not implemented" (* TODO: Реализовать ветвление *)
  | CApp (func, args) ->
    let f = codegen_immexpr func in
    let arg_vals = Array.of_list (List.map ~f:codegen_immexpr args) in
    build_call (type_of f) f arg_vals "calltmp" (builder context)
;;

let rec codgen_aexpr (ae : aexpr) : llvalue =
  match ae with
  | ACExpr ce -> codegen_cexpr ce
  | ALetIn (id, ce, body) ->
    let builder = builder context in
    let init_val = codegen_cexpr ce in
    let addr = build_alloca i64 id builder in
    ignore (build_store init_val addr builder);
    Hashtbl.set env ~key:id ~data:addr;
    codgen_aexpr body
;;

let codegen_decl (decl : adecl) : unit =
  match decl with
  | ADLet (_, id, args, body) ->
    if List.is_empty args
    then (
      (* Обработка глобальных переменных *)
      try
        let init_val = codgen_aexpr body in
        let _global = define_global id init_val the_module in
        ()
      with
      | Failure _ ->
        let placeholder = const_int i64 0 in
        let _global_var = define_global id placeholder the_module in
        global_inits := (id, fun () -> codgen_aexpr body) :: !global_inits)
    else (
      (* Генерация функции *)
      let fnty = function_type i64 (Array.create ~len:(List.length args) i64) in
      let func = declare_function id fnty the_module in
      (* Hashtbl.set env ~key:id ~data:func; *)
      let bb = append_block context "entry" func in
      let func_builder = builder_at_end context bb in
      List.iteri args ~f:(fun i (arg_id, _) ->
        let arg_val = param func i in
        let addr = build_alloca i64 arg_id func_builder in
        ignore (build_store arg_val addr func_builder);
        Hashtbl.set env ~key:arg_id ~data:addr);
      let ret_val = codgen_aexpr body in
      ignore (build_ret ret_val func_builder))
;;

let create_global_init_function () =
  let init_ty = function_type i64 [||] in
  let init_fn = define_function "global_init" init_ty the_module in
  let entry_bb = append_block context "entry" init_fn in
  let init_builder = builder_at_end context entry_bb in
  List.iter !global_inits ~f:(fun (id, compute) ->
    let value = compute () in
    match lookup_global id the_module with
    | Some v -> ignore (build_store value v init_builder)
    | None -> failwith ("Global variable " ^ id ^ " not found"));
  ignore (build_ret (const_int i64 0) init_builder);
  init_fn
;;

let codegen_program (prog : aprogram) : unit = List.iter prog ~f:codegen_decl

let generate (prog : aprogram) : llmodule =
  codegen_program prog;
  the_module
;;

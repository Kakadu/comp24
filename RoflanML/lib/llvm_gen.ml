open Llvm
open Ast
open Anf_ast
open Base

let context = global_context ()
let the_module = create_module context "Roflan"
let builder = builder context
let i64 = i64_type context
let env : (id, llvalue) Hashtbl.t = Hashtbl.create (module String)

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
     | Some addr -> build_load i64 addr id builder
     | None -> failwith ("Undefined variable: " ^ id))
  | ImmTuple _ -> failwith "Tuple generation not implemented" (* TODO *)
  | ImmList _ -> failwith "List generation not implemented" (* TODO *)
;;

let rec codegen_cexpr (ce : cexpr) : llvalue =
  match ce with
  | CImm imm -> codegen_immexpr imm
  | CBranch _ (* (cond, then_ae, else_ae)*) ->
    failwith "Branch generation not implemented" (* TODO *)
  | CApp (func, args) ->
    let f = codegen_immexpr func in
    let arg_vals = Array.of_list (List.map ~f:codegen_immexpr args) in
    build_call (type_of f) f arg_vals "calltmp" builder

and codgen_aexpr (ae : aexpr) : llvalue =
  match ae with
  | ACExpr ce -> codegen_cexpr ce
  | ALetIn (id, ce, body) ->
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
      let init_val =
        match body with
        | ACExpr ce -> codegen_cexpr ce
        | _ -> failwith "Only constant initializers are supported for global variables."
      in
      let _global = define_global id init_val the_module in
      ())
    else (
      let fnty = function_type i64 (Array.create ~len:(List.length args) i64) in
      let func = declare_function id fnty the_module in
      let bb = append_block context "entry" func in
      position_at_end bb builder;
      List.iteri args ~f:(fun i (arg_id, _) ->
        let arg_val = param func i in
        let addr = build_alloca i64 arg_id builder in
        ignore (build_store arg_val addr builder);
        Hashtbl.set env ~key:arg_id ~data:addr);
      let ret_val = codgen_aexpr body in
      ignore (build_ret ret_val builder))
;;

let codegen_program (prog : aprogram) : unit = List.iter prog ~f:codegen_decl

let generate (prog : aprogram) : llmodule =
  codegen_program prog;
  the_module
;;

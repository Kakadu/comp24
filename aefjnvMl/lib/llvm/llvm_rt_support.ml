(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm
open Llvm_monad

module Runtime (Ctx : LlvmGlobalCtx) = struct
  open Common
  open LlvmMonad (Ctx)
  open Base_lib.LibF

  let tp_to_lltp =
    let open Ast in
    function
    | Ptyp_unit | Ptyp_var _ | Ptyp_int | Ptyp_bool -> return Ctx.i64_t
    | _ -> fail "Unsupported base_lib func type: no need"
  ;;

  let create_func_type arg_tps ret_tp =
    let open Common.Base_lib.LibF in
    let args_tp_arr = Array.of_list arg_tps in
    function
    | NoVarargs -> function_type ret_tp args_tp_arr
    | Varargs _ -> var_arg_function_type ret_tp args_tp_arr
  ;;

  (* name -> Alias ... *)
  let declare_std_aliased_func f =
    let name = get_name f in
    let alias = get_rt_alias f in
    let vararg_tp, (args_tp, ret_tp) = get_row_type f in
    let* args = mapt args_tp tp_to_lltp in
    let args_n = List.length args in
    let* ret = tp_to_lltp ret_tp in
    let f_tp = create_func_type args ret vararg_tp in
    ignore_t @@ aliased_declare_func name alias args_n f_tp
  ;;

  (* DEF values *)
  let declare_stdlib_functions = mapt Base_lib.base_lib_decls declare_std_aliased_func

  let gen_id =
    let open Match_elimination.Me_ast in
    function
    | Id_name nm -> return nm
    | Id_unit ->
      let+ fresh_n = fresh in
      Naming.runtime_prefix ^ Int.to_string fresh_n
  ;;

  let tag_int i = (i lsl 1) + 1
  let rt_int value = const_int Ctx.i64_t (tag_int value)
  let null : llvalue = const_int Ctx.i64_t 0

  (* WRAPPERS *)

  let gen_rt_int builder i =
    Llvm.build_add
      (Llvm.build_shl i (Llvm.const_int Ctx.i64_t 1) "" builder)
      (Llvm.const_int Ctx.i64_t 1)
      ""
      builder
  ;;

  let untag_rt_int builder i = Llvm.build_ashr i (Llvm.const_int Ctx.i64_t 1) "" builder

  let gen_icmp cmode val1 val2 =
    let* builder = get_builder in
    let bool1 = Llvm.build_icmp cmode val1 val2 "" builder in
    let bool64 = Llvm.build_zext bool1 Ctx.i64_t "" builder in
    return @@ gen_rt_int builder bool64
  ;;

  (* SYS funcs *)

  let call_empty_closure_alloc name =
    let* _, args_n, f = lookup_func name in
    let* builder = get_builder in
    let casted_f = build_ptrtoint f Ctx.i64_t "" builder in
    call_declared "" (get_name create_empty_closure) [ casted_f; rt_int args_n ]
  ;;

  let call_tuple_alloc n values =
    call_declared "" (get_name alloc_tuple) @@ (rt_int n :: values)
  ;;

  let call_append_to_list n tl reversed_args_to_append =
    call_declared "" (get_name append_to_list)
    @@ (rt_int n :: tl :: reversed_args_to_append)
  ;;

  let call_apply_args_to_closure cl args =
    let len = List.length args in
    call_declared "" (get_name apply_arguments) @@ (cl :: rt_int len :: args)
  ;;
end

(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm

module type LlvmGlobalCtx = sig
  val ctx : llcontext
  val g_module : llmodule
  val void_t : lltype
  val i64_t : lltype
end

module LlvmMonad (Ctx : LlvmGlobalCtx) = struct
  module Defined_Map = Stdlib.Map.Make (String)

  type vb_map = llvalue Defined_Map.t
  type gvb_map = llvalue Defined_Map.t
  type llfunc_map = (lltype * int * llvalue) Defined_Map.t

  include
    Common.Monads.GenericCounterMonad
      (struct
        type t = string * llbuilder * (llfunc_map * vb_map * gvb_map)
      end)
      (String)

  (* ============ MEMORY ============ *)

  let get_builder : llbuilder t =
    let* _, b, _ = read in
    return b
  ;;

  let get_cur_func : string t =
    let* f, _, _ = read in
    return f
  ;;

  let set_cur_func name =
    let* _, a, b = read in
    save (name, a, b)
  ;;

  let save_builder : llbuilder -> unit t =
    fun builder ->
    let* cur_f, _, maps = read in
    save (cur_f, builder, maps)
  ;;

  let save_fun_def name tp =
    let* cur_f, llvm_st, (def_tps, local_vars, glob_vars) = read in
    save (cur_f, llvm_st, (Defined_Map.add name tp def_tps, local_vars, glob_vars))
  ;;

  (* REFACTOR IT ????? *)

  let load_local_var_opt nm =
    let+ _, _, (_, scope_values, _) = read in
    Defined_Map.find_opt nm scope_values
  ;;

  let load_var nm =
    let* var_opt = load_local_var_opt nm in
    match var_opt with
    | Some value -> return value
    | None -> fail @@ "No name in scope: " ^ nm
  ;;

  let store_local_var nm value =
    let* cur_f, a, (f_map, scope_values, glob_vars) = read in
    let scope_values' = Defined_Map.add nm value scope_values in
    save (cur_f, a, (f_map, scope_values', glob_vars))
  ;;

  let load_global_var nm =
    let* _, _, (_, _, glob_vars) = read in
    match Defined_Map.find_opt nm glob_vars with
    | Some value -> return value
    | None -> fail @@ "No name in scope: " ^ nm
  ;;

  let store_global_var nm value =
    let* cur_f, a, (f_map, scope_values, glob_vars) = read in
    let glob_vars' = Defined_Map.add nm value glob_vars in
    save (cur_f, a, (f_map, scope_values, glob_vars'))
  ;;

  (* ============ BUILDERS & BLOCKS ============ *)

  let select_bb : llbasicblock -> unit t =
    fun bb ->
    let+ builder' = get_builder in
    ignore @@ position_at_end bb builder'
  ;;

  let in_new_bb bb f =
    let* old_builder = get_builder in
    let* () =
      let new_builder = builder_at Ctx.ctx (instr_begin bb) in
      save_builder new_builder
    in
    let* () = select_bb bb in
    let* f_res = f in
    let+ () = save_builder old_builder in
    f_res
  ;;

  (* ============ FUNCTIONS ============ *)

  let aliased_declare_func key_name name args_n fun_tp =
    let f = declare_function name fun_tp Ctx.g_module in
    save_fun_def key_name (fun_tp, args_n, f)
  ;;

  let declare_func name = aliased_declare_func name name

  let lookup_func : string -> (lltype * int * llvalue) t =
    fun fn_name ->
    let* _, _, (defs, _, _) = read in
    match Defined_Map.find_opt fn_name defs with
    | Some tp -> return tp
    | None -> fail @@ "Type of {" ^ fn_name ^ "} not found"
  ;;

  let call_declared : string -> string -> llvalue list -> llvalue t =
    fun return_name fn_name params ->
    let params_arr = Array.of_list params in
    let* fn_tp, _, fn = lookup_func fn_name in
    let+ builder = get_builder in
    build_call fn_tp fn params_arr return_name builder
  ;;

  let void_call_declared = call_declared ""

  let add_ret : llvalue -> unit t =
    fun rc ->
    let+ builder = get_builder in
    ignore @@ build_ret rc builder
  ;;

  (* ============ UTILS ============ *)

  let create_global_var name =
    Llvm.define_global name (const_int Ctx.i64_t 0) Ctx.g_module
  ;;

  let localize_global_var global_var local_name =
    let* builder = get_builder in
    let local_var = build_load Ctx.i64_t global_var local_name builder in
    let+ () = store_local_var local_name local_var in
    local_var
  ;;

  let new_func_scope f_name ent_bb f =
    let* f_name_old, _, (_, vb_map_old, _) = read in
    let* () = set_cur_func f_name in
    let* res = in_new_bb ent_bb f in
    let* _, a, (b, _, c) = read in
    let+ () = save (f_name_old, a, (b, vb_map_old, c)) in
    res
  ;;

  let check_func f_name func =
    match Llvm_analysis.verify_function func with
    | false -> fail @@ "The generated code has an error in func:" ^ f_name
    | true -> return func
  ;;
end

(** Copyright 2024, Artem-Rzhankoff, ItIsMrLag *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Llvm
open Llvm_monad
open Llvm_rt_support

module LlvmCompiler (Ctx : LlvmGlobalCtx) = struct
  open LlvmMonad (Ctx)
  open Runtime (Ctx)
  open Anf_conversion.Anf_ast

  let gen_scoped_name = function
    | Local_id name -> load_var name
    | Global_func name -> call_empty_closure_alloc name
    | Global_var name ->
      let* cache_var = load_local_var_opt name in
      (match cache_var with
       | Some x -> return x
       | None ->
         let* global_var = load_global_var name in
         localize_global_var global_var name)
  ;;

  let gen_immexpr iexpr =
    match iexpr with
    | Imm_int i -> return @@ rt_int i
    | Imm_bool b -> return @@ rt_int @@ Bool.to_int b
    | Imm_unit | Imm_nil | Imm_id Id_unit -> return @@ null
    | Imm_id (Id_name scoped_id) -> gen_scoped_name scoped_id
  ;;

  let is_call_ready ready_args_n = function
    | Global_func name ->
      let+ _, full_args_n, _ = lookup_func name in
      (match ready_args_n = full_args_n with
       | true -> Some name
       | false -> None)
    | _ -> return None
  ;;

  let rec gen_cexpr : cexpr -> llvalue t = function
    | C_immexpr imme -> gen_immexpr imme
    | C_tuple imm'l ->
      let* llval'l = mapt imm'l gen_immexpr in
      let n = List.length llval'l in
      call_tuple_alloc n llval'l
    | C_rlist (Imm_nil, []) -> return @@ null
    | C_rlist (tl, rpref) ->
      let* tl' = gen_immexpr tl in
      let* args = mapt rpref gen_immexpr in
      call_append_to_list (List.length args) tl' args
    | C_apply (f, fst_arg, arg'l) ->
      let* fst_arg' = gen_immexpr fst_arg in
      let* args' = mapt arg'l gen_immexpr in
      let all_args = fst_arg' :: args' in
      (match f, all_args with
       | Global_func "==", [ arg1; arg2 ] -> gen_icmp Icmp.Eq arg1 arg2
       | Global_func "&&", [ arg1; arg2 ] ->
         let+ builder = get_builder in
         let untaged_arg1 = untag_rt_int builder arg1 in
         let untaged_arg2 = untag_rt_int builder arg2 in
         build_and untaged_arg1 untaged_arg2 "" builder
       | Global_func "||", [ arg1; arg2 ] ->
         let+ builder = get_builder in
         let untaged_arg1 = untag_rt_int builder arg1 in
         let untaged_arg2 = untag_rt_int builder arg2 in
         build_or untaged_arg1 untaged_arg2 "" builder
       | f_name, args ->
         let args_for_apply_n = List.length args in
         let* mb_call_name = is_call_ready args_for_apply_n f_name in
         (match mb_call_name with
          | Some f_name' -> call_declared "" f_name' args
          | None ->
            let* closure = gen_scoped_name f_name in
            call_apply_args_to_closure closure @@ all_args))
    | C_ifthenelse (cond, t, e) ->
      let* cur_builder = get_builder in
      let* cond' = gen_immexpr cond in
      let cond_i1 =
        Llvm.build_trunc
          (untag_rt_int cur_builder cond')
          (Llvm.i1_type Ctx.ctx)
          ""
          cur_builder
      in
      let* cur_func =
        let* cur_fun_name = get_cur_func in
        let+ _, _, func = lookup_func cur_fun_name in
        func
      in
      let contin_bb = append_block Ctx.ctx "continuation" cur_func in
      let gen_branch body =
        let branch_block = append_block Ctx.ctx "" cur_func in
        in_new_bb branch_block
        @@
        let* aexp' = gen_aexpr body in
        let+ builder = get_builder in
        let last_inst = build_br contin_bb builder in
        (aexp', instr_parent last_inst), branch_block
      in
      let* then_mb_res, then_bb = gen_branch t in
      let+ else_mb_res, else_bb = gen_branch e in
      let _ = build_cond_br cond_i1 then_bb else_bb cur_builder in
      let _ = position_at_end contin_bb cur_builder in
      build_phi [ then_mb_res; else_mb_res ] "" cur_builder

  and gen_aexpr = function
    | A_cexpr cexp -> gen_cexpr cexp
    | A_let (name, cexpr, cont) ->
      let* cexpr' = gen_cexpr cexpr in
      (match name with
       | Id_unit -> gen_aexpr cont
       | Id_name nm ->
         let* () = store_local_var nm cexpr' in
         gen_aexpr cont)
  ;;

  let declare_user_func { adec_name; adec_args; _ } =
    let arg1, argsn = adec_args in
    let args_n = List.length (arg1 :: argsn) in
    let args_arr = Array.init args_n (fun _ -> Ctx.i64_t) in
    let fun_tp = function_type Ctx.i64_t args_arr in
    declare_func adec_name args_n fun_tp
  ;;

  let define_user_func { adec_name; adec_args; adec_body } =
    let* _, _, func = lookup_func adec_name in
    let entry_bb = append_block Ctx.ctx "entry" func in
    let arg1, argsn = adec_args in
    let* _ =
      new_func_scope adec_name entry_bb
      @@
      let* builder = get_builder in
      let args_vals = Array.to_list (Llvm.params func) in
      let* args_names = mapt (arg1 :: argsn) gen_id in
      let name_and_val = List.combine args_names args_vals in
      let* _ = mapt name_and_val (fun (nm, llval) -> store_local_var nm llval) in
      let+ res = gen_aexpr adec_body in
      Llvm.build_ret res builder
    in
    check_func adec_name func
  ;;

  (* Should be run only in main function *)
  let gen_anf_decl = function
    | A_NonrecDecl func ->
      let* _ = declare_user_func func in
      ignore_t @@ define_user_func func
    | A_RecDecl (func1, funcn) ->
      let all_funcs = func1 :: funcn in
      let* _ = mapt all_funcs declare_user_func in
      ignore_t @@ mapt all_funcs define_user_func
      (* All global bindings declare as global variables, but defined only in main scope *)
    | A_GlobalV (bind_name, expr) ->
      let* bind_name = gen_id bind_name in
      let global_var = create_global_var bind_name in
      let* () = store_global_var bind_name global_var in
      let* expr' = gen_aexpr expr in
      ignore_t
      @@
      let+ builder = get_builder in
      build_store expr' global_var builder
  ;;

  let gen_prog prog = mapt prog gen_anf_decl

  let compile_llvm_common prog =
    let open Common.Naming in
    let main_func_tp = function_type Ctx.i64_t [||] in
    let main_func = declare_function rt_'main' main_func_tp Ctx.g_module in
    let entry_bb = append_block Ctx.ctx "entry" main_func in
    let builder = builder_at Ctx.ctx (instr_begin entry_bb) in
    let res =
      let st =
        let func_map =
          Defined_Map.add rt_'main' (main_func_tp, 0, main_func) Defined_Map.empty
        in
        rt_'main', builder, (func_map, Defined_Map.empty, Defined_Map.empty)
      in
      run
        (let* _ = declare_stdlib_functions in
         let* _ = in_new_bb entry_bb @@ gen_prog prog in
         let _ = build_ret (const_int Ctx.i64_t 0) builder in
         check_func rt_'main' main_func)
        0
        st
    in
    res
  ;;
end

open Ast
open Typing
open Compiler_utils
open Anf

let the_context = Llvm.global_context ()
let the_module = Llvm.create_module the_context "HamsterML"
let i64_t = Llvm.i64_type the_context
let llvm_suffix = "_llvm"
let nat2ml nat = (nat lsl 1) + 1

type llvm_name = string
type name = string

type is_system =
  | UserFun
  | SystemFun

type is_vararg =
  | NotVararg
  | Vararg

type fun_type = is_system * is_vararg
type std_fun = name * llvm_name * fun_type * inf_type

let default_type = UserFun, NotVararg
let bin_op arg1 arg2 ret = TArrow (arg1, TArrow (arg2, ret))
let universal_function = TArrow (TPVar 1, TPVar 2)

let stdlib_funs : std_fun list =
  [ "( + )", "plus_mlint", default_type, bin_op TInt TInt TInt
  ; "( - )", "minus_mlint", default_type, bin_op TInt TInt TInt
  ; "( * )", "mult_mlint", default_type, bin_op TInt TInt TInt
  ; "( / )", "div_mlint", default_type, bin_op TInt TInt TInt
  ; "( < )", "l_ml", default_type, bin_op (TPVar 1) (TPVar 1) TBool
  ; "( <= )", "le_ml", default_type, bin_op (TPVar 1) (TPVar 1) TBool
  ; "( > )", "g_ml", default_type, bin_op (TPVar 1) (TPVar 1) TBool
  ; "( >= )", "ge_ml", default_type, bin_op (TPVar 1) (TPVar 1) TBool
  ; "( = )", "eq_ml", default_type, bin_op (TPVar 1) (TPVar 1) TBool
  ; "( == )", "peq_ml", default_type, bin_op (TPVar 1) (TPVar 1) TBool
  ; "( <> )", "neq_ml", default_type, bin_op (TPVar 1) (TPVar 1) TBool
  ; "( != )", "pneq_ml", default_type, bin_op (TPVar 1) (TPVar 1) TBool
  ; "print_int", "print_int", default_type, TArrow (TInt, TUnit)
  ; "( && )", "land_ml", default_type, bin_op TBool TBool TBool
  ; "( || )", "lor_ml", default_type, bin_op TBool TBool TBool
  ; ( "get_field"
    , "mlrt_get_box_field"
    , (SystemFun, NotVararg)
    , TArrow (TPVar 1, TArrow (TInt, TPVar 2)) )
  ; ( "check_tag"
    , "mlrt_check_tag"
    , (SystemFun, NotVararg)
    , TArrow (TPVar 1, TArrow (TInt, TBool)) )
  ; "match_error", "mltr_match_error", (SystemFun, NotVararg), TArrow (TUnit, TPVar 1)
  ; "_create_tuple", "mlrt_create_tuple", (SystemFun, Vararg), TArrow (TInt, TPVar 1)
  ; ( "_create_empty_closure"
    , "mlrt_create_empty_closure"
    , (SystemFun, NotVararg)
    , universal_function )
  ; ( "_apply_args_to_closure"
    , "mlrt_apply_args_to_closure"
    , (SystemFun, Vararg)
    , TArrow (TPVar 1, TArrow (TInt, TPVar 2)) )
  ]
;;

let build_nat2ml : Llvm.llbuilder -> Llvm.llvalue -> Llvm.llvalue =
  fun builder nat ->
  Llvm.build_add
    (Llvm.build_shl nat (Llvm.const_int i64_t 1) "" builder)
    (Llvm.const_int i64_t 1)
    ""
    builder
;;

let build_ret_mlvoid = Llvm.build_ret (Llvm.const_int i64_t 0)
let help_globname_by_orig name = Format.sprintf "%s_glob%s" name llvm_suffix

let create_help_global name =
  Llvm.define_global (help_globname_by_orig name) (Llvm.const_int i64_t 0) the_module
;;

let count_args fun_tp =
  let rec function_deep acc = function
    | TArrow (_arg_tp, ret_tp) -> function_deep (acc + 1) ret_tp
    | _ -> acc
  in
  function_deep 0 fun_tp
;;

let gen_function_type_std tp vararg_flag =
  let args_cnt = count_args tp in
  let args_arr = Array.init args_cnt (fun _ -> i64_t) in
  match vararg_flag with
  | NotVararg -> Llvm.function_type i64_t args_arr
  | Vararg -> Llvm.var_arg_function_type i64_t args_arr
;;

let declare_std_fun : std_fun -> (state, Llvm.llvalue) t =
  fun (_, name, (_, vararg_flag), tp) ->
  let fun_tp = gen_function_type_std tp vararg_flag in
  let fun_val = Llvm.declare_function name fun_tp the_module in
  add_glob_fun name fun_tp fun_val *> return fun_val
;;

let declare_function name args_cnt =
  let args_arr = Array.init args_cnt (fun _ -> i64_t) in
  let fun_tp = Llvm.function_type i64_t args_arr in
  let fun_val = Llvm.declare_function name fun_tp the_module in
  add_glob_fun name fun_tp fun_val *> return fun_val
;;

let declare_all_std_funs : unit -> (state, Llvm.llvalue list) t =
  fun () -> map_list declare_std_fun stdlib_funs
;;

let find_fun : string -> (state, Llvm.lltype * Llvm.llvalue) t =
  fun name ->
  let* find_res = findopt_glob_fun name in
  match find_res with
  | Some x -> return x
  | None -> fail (Format.sprintf "Can't find function %s" name)
;;

let find_glob : string -> (state, Llvm.llvalue) t =
  fun name ->
  let find_res = Llvm.lookup_global name the_module in
  match find_res with
  | Some x -> return x
  | None -> fail (Format.sprintf "Can't find global %s" name)
;;

let build_fun_call
  : string -> Llvm.llvalue array -> Llvm.llbuilder -> (state, Llvm.llvalue) t
  =
  fun name args builder ->
  let* fun_tp, fun_llvm = find_fun name in
  return (Llvm.build_call fun_tp fun_llvm args "" builder)
;;

let build_mlrt_apply_args_to_closure
  : Llvm.llvalue -> Llvm.llvalue array -> Llvm.llbuilder -> (state, Llvm.llvalue) t
  =
  fun closure applied_args builder ->
  let applied_args_cnt = Array.length applied_args in
  let args =
    Array.append [| closure; Llvm.const_int i64_t applied_args_cnt |] applied_args
  in
  build_fun_call "mlrt_apply_args_to_closure" args builder
;;

let build_mlrt_create_empty_closure
  : Llvm.llvalue -> int -> Llvm.llbuilder -> (state, Llvm.llvalue) t
  =
  fun target_fun args_count builder ->
  let int_ptr = Llvm.build_ptrtoint target_fun i64_t "" builder in
  let* clos =
    build_fun_call
      "mlrt_create_empty_closure"
      [| int_ptr; Llvm.const_int i64_t args_count |]
      builder
  in
  return clos
;;

let declare_and_define_rt_globs : Llvm.llbuilder -> (state, unit) t =
  fun builder ->
  let help : std_fun -> (state, unit) t =
    fun (_, name, (sys_f, _), fun_ml_tp) ->
    match sys_f with
    | UserFun ->
      let glob_c = create_help_global name in
      let* _, target_fun = find_fun name in
      let* clos =
        build_mlrt_create_empty_closure target_fun (count_args fun_ml_tp) builder
      in
      let _ = Llvm.build_store clos glob_c builder in
      return ()
    | _ -> return ()
  in
  map_list help stdlib_funs *> return ()
;;

let create_init : unit -> (state, Llvm.llvalue) t =
  fun () ->
  let init_name = Format.sprintf "init%s" llvm_suffix in
  let* init_fun = declare_function init_name 0 in
  let entry_b = Llvm.append_block the_context "entry" init_fun in
  let builder = Llvm.builder_at the_context (Llvm.instr_begin entry_b) in
  new_fun_scope
    init_name
    (let* _ = declare_and_define_rt_globs builder in
     let _ = build_ret_mlvoid builder in
     return init_fun)
;;

let localise_global_var : string -> Llvm.llvalue -> (state, Llvm.llvalue) t =
  fun loc_name glob_var ->
  let* curr_fun = read_curr_fun in
  let* tp_val = find_fun curr_fun in
  let _, llfun = tp_val in
  let eb = Llvm.entry_block llfun in
  let builder = Llvm.builder_at the_context (Llvm.instr_begin eb) in
  let loc_var = Llvm.build_load i64_t glob_var loc_name builder in
  add_loc_var loc_name loc_var *> return loc_var
;;

let std_renaming_table =
  List.fold_left
    (fun acc (ml_name, llvm_name, _, _) -> MapString.add ml_name llvm_name acc)
    MapString.empty
    stdlib_funs
;;

let std_renaming old_name =
  match MapString.find_opt old_name std_renaming_table with
  | Some new_name -> new_name
  | None -> old_name
;;

let build_identifier : string -> (state, Llvm.llvalue) t =
  fun idname ->
  let* find_res = findopt_loc_var idname in
  match find_res with
  | Some llval -> return llval
  | None ->
    (* Add special handling for operators *)
    let op_name =
      match idname with
      | "<=" -> "( <= )"
      | ">=" -> "( >= )"
      | "<" -> "( < )"
      | ">" -> "( > )"
      | "=" -> "( = )"
      | "==" -> "( == )"
      | "!=" -> "( != )"
      | "<>" -> "( <> )"
      | "+" -> "( + )"
      | "-" -> "( - )"
      | "*" -> "( * )"
      | "/" -> "( / )"
      | "&&" -> "( && )"
      | "||" -> "( || )"
      | _ -> idname
    in
    let renamed_id = std_renaming op_name in
    let glob_name = help_globname_by_orig renamed_id in
    let* glob_val = find_glob glob_name in
    localise_global_var idname glob_val
;;

let build_imm_expr : Llvm.llbuilder -> imm_expr -> (state, Llvm.llvalue) t =
  fun _ -> function
  | ImmBool b -> return (Llvm.const_int i64_t (nat2ml (Bool.to_int b)))
  | ImmInt i ->
    (* Use consistent encoding for all integers *)
    return (Llvm.const_int i64_t (nat2ml i))
  | ImmId id_name -> build_identifier id_name
  | ImmString s -> return (Llvm.const_int i64_t (nat2ml (String.length s)))
  | ImmUnit -> return (Llvm.const_int i64_t 1) (* ML encoded 0 is 1 *)
  | ImmTuple _ | ImmList _ -> failwith "Lists and tuples are not implemented in codegen"
  | ImmOperation op ->
    (match op with
     | Binary bop -> build_identifier (BinOperator.to_string bop)
     | Unary uop -> build_identifier (UnOperator.to_string uop))
;;

let build_icmp_ml
  : Llvm.llbuilder -> Llvm.Icmp.t -> Llvm.llvalue -> Llvm.llvalue -> Llvm.llvalue
  =
  fun builder cmode val1 val2 ->
  let bool1 = Llvm.build_icmp cmode val1 val2 "" builder in
  let bool64 = Llvm.build_zext bool1 i64_t "" builder in
  build_nat2ml builder bool64
;;

let build_apply_closure
  : Llvm.llbuilder -> Llvm.llvalue -> Llvm.llvalue list -> (state, Llvm.llvalue) t
  =
  fun builder fun_c args_val ->
  build_mlrt_apply_args_to_closure fun_c (Array.of_list args_val) builder
;;

let cimm_list_to_imm_list lst =
  let helper = function
    | CImm imm -> imm
    | _ -> failwith "No other cexpr allowed"
  in
  List.map helper lst
;;

let rec build_cexpr : Llvm.llbuilder -> cexpr -> (state, Llvm.llvalue) t =
  fun builder -> function
  | CImm imm -> build_imm_expr builder imm
  | CApplication (CImm (ImmOperation op), CImm imm_arg_hd, []) ->
    (match op with
     | Unary UMINUS ->
       let* arg = build_imm_expr builder imm_arg_hd in
       (* For ML-encoded integers (n lsl 1) + 1, negation should be (-n lsl 1) + 1
          which equals -((n lsl 1) + 1) + 2 or -arg + 2 *)
       return
         (Llvm.build_add
            (Llvm.build_neg arg "" builder)
            (Llvm.const_int i64_t 2)
            ""
            builder)
     | _ -> failwith "No other unary operators allowed")
  | CApplication (CImm (ImmId fun_name as imm_fun_c), CImm imm_arg_hd, imm_arg_tl) ->
    let imm_arg_tl = cimm_list_to_imm_list imm_arg_tl in
    let* args_val = map_list (build_imm_expr builder) (imm_arg_hd :: imm_arg_tl) in
    (* Optimization for simple operations *)
    (match fun_name, args_val with
     | "( && )", [ a; b ] -> return (Llvm.build_and a b "" builder)
     | "( || )", [ a; b ] -> return (Llvm.build_or a b "" builder)
     | "( == )", [ a; b ] -> return (build_icmp_ml builder Llvm.Icmp.Eq a b)
     | "( != )", [ a; b ] -> return (build_icmp_ml builder Llvm.Icmp.Ne a b)
     | _, _ ->
       (* Optimize full args function call*)
       let fun_name = std_renaming fun_name in
       let* find_res = findopt_glob_fun fun_name in
       (match find_res with
        | Some (_fun_tp, fun_val)
          when Array.length (Llvm.params fun_val) = List.length args_val ->
          build_fun_call fun_name (Array.of_list args_val) builder
        | _ ->
          let* fun_c = build_imm_expr builder imm_fun_c in
          build_apply_closure builder fun_c args_val))
  | CApplication (fun_expr, arg_hd, arg_tl) ->
    (* Evaluate arguments from right to left *)
    let rev_args = List.rev arg_tl in
    let* arg_tl_vals_rev = map_list (build_cexpr builder) rev_args in
    let* arg_hd_val = build_cexpr builder arg_hd in
    let* fun_val = build_cexpr builder fun_expr in
    (* Reconstruct arguments in original order *)
    let arg_vals = arg_hd_val :: List.rev arg_tl_vals_rev in
    build_apply_closure builder fun_val arg_vals
  | CIf (imm_flag, then_body, else_body) ->
    let* flag = build_imm_expr builder imm_flag in
    let bool1 =
      Llvm.build_trunc
        (Llvm.build_ashr flag (Llvm.const_int i64_t 1) "" builder)
        (Llvm.i1_type the_context)
        ""
        builder
    in
    let* curr_fun_name = read_curr_fun in
    let* fun_tp_llval = find_fun curr_fun_name in
    let _, fun_val = fun_tp_llval in
    let continue_blck = Llvm.append_block the_context "continue" fun_val in
    let create_branch branch_body =
      let branch_block = Llvm.append_block the_context "" fun_val in
      let branch_builder = Llvm.builder_at the_context (Llvm.instr_begin branch_block) in
      let* res = build_aexpr branch_builder branch_body in
      let last_inst = Llvm.build_br continue_blck branch_builder in
      return ((res, Llvm.instr_parent last_inst), branch_block)
    in
    let* then_r_lb, then_fb = create_branch then_body in
    let* else_r_lb, else_fb =
      match else_body with
      | Some body -> create_branch body
      | None -> create_branch (ACExpr (CImm ImmUnit))
    in
    let _ = Llvm.build_cond_br bool1 then_fb else_fb builder in
    let _ = Llvm.position_at_end continue_blck builder in
    return (Llvm.build_phi [ then_r_lb; else_r_lb ] "" builder)
  | CConstructList (_, _) -> failwith "Lists are not implemented in codegen"

and build_aexpr : Llvm.llbuilder -> aexpr -> (state, Llvm.llvalue) t =
  fun builder -> function
  | ACExpr cexp -> build_cexpr builder cexp
  | ALetIn (Var nm, nm_val, body) ->
    let* nm_llval = build_cexpr builder nm_val in
    add_loc_var nm nm_llval *> build_aexpr builder body
  | ALetIn (Wildcard, nm_val, body) ->
    let* _ = build_cexpr builder nm_val in
    build_aexpr builder body
  | ALetIn (Const Unit, nm_val, body) ->
    let* _ = build_cexpr builder nm_val in
    build_aexpr builder body
  | ALetIn (_, _, _) ->
    failwith
      "Unsupported pattern in let binding (only variable and wildcard patterns are \
       supported)"
;;

let decl_sindle_anf_bind : single_anf_binding -> (state, Llvm.llvalue * Llvm.llvalue) t =
  fun (ALet (name, args_name, _)) ->
  let* llfun = declare_function name (List.length args_name) in
  let fun_c = create_help_global name in
  return (llfun, fun_c)
;;

let define_sindle_anf_bind
  : Llvm.llbuilder -> single_anf_binding -> (state, Llvm.llvalue * Llvm.llvalue) t
  =
  fun builder (ALet (name, args_name, body)) ->
  let* val_and_tp = find_fun name in
  let fun_tp, fun_val = val_and_tp in
  let entry_b = Llvm.append_block the_context "entry" fun_val in
  let new_builder = Llvm.builder_at the_context (Llvm.instr_begin entry_b) in
  let* _ =
    new_fun_scope
      name
      (let args_val = Array.to_list (Llvm.params fun_val) in
       let name_and_val = List.combine args_name args_val in
       let* _ = map_list (fun (nm, llval) -> add_loc_var nm llval) name_and_val in
       let* res = build_aexpr new_builder body in
       return (Llvm.build_ret res new_builder))
  in
  (* define helper var*)
  let helper_name = help_globname_by_orig name in
  let args_cnt = List.length args_name in
  let* helper_val =
    if args_cnt = 0
    then return (Llvm.build_call fun_tp fun_val [||] helper_name builder)
    else build_mlrt_create_empty_closure fun_val args_cnt builder
  in
  let* helper_ptr = find_glob helper_name in
  let _ = Llvm.build_store helper_val helper_ptr builder in
  return (fun_val, helper_ptr)
;;

let build_anf_decl : Llvm.llbuilder -> anf_decl -> (state, unit) t =
  fun builder -> function
  | ADSingleLet (_, slet) ->
    let* _ = decl_sindle_anf_bind slet in
    let* _ = define_sindle_anf_bind builder slet in
    return ()
  | ADMutualRecDecl slet_lst ->
    let* _ = map_list decl_sindle_anf_bind slet_lst in
    let* _ = map_list (define_sindle_anf_bind builder) slet_lst in
    return ()
;;

let create_main : Llvm.llvalue -> anf_prog -> (state, unit) t =
  fun init_fun anf_decl_lst ->
  let main_name = "main" in
  let* main_fun = declare_function main_name 0 in
  let entry_b = Llvm.append_block the_context "entry" main_fun in
  let builder = Llvm.builder_at the_context (Llvm.instr_begin entry_b) in
  new_fun_scope
    main_name
    (let _ = Llvm.build_call (Llvm.function_type i64_t [||]) init_fun [||] "" builder in
     let* _ = map_list (build_anf_decl builder) anf_decl_lst in
     let _ = build_ret_mlvoid builder in
     return ())
  *> return ()
;;

let start_codegen : anf_prog -> (state, unit) t =
  fun prog ->
  let* init_fun = declare_all_std_funs () *> create_init () in
  create_main init_fun prog
;;

let codegen out_name prog =
  let _, res = run (start_codegen prog) ("", MapString.empty, MapString.empty) in
  match res with
  | Ok _ -> Llvm.print_module out_name the_module
  | Error e -> Format.printf "Get error: %s" e
;;

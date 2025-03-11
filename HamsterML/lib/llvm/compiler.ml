open Llvm
open Ast
open Anf

(* LLVM setup *)
let global_context = global_context ()
let builder = builder global_context
let the_module = create_module global_context "HamsterML_LLVM_Compiler"
let i64 = i16_type global_context
let lookup_function_exception id llmodule = Option.get @@ lookup_function id llmodule

let build_binary_operation = function
  | ADD -> build_add
  | SUB -> build_sub
  | MUL -> build_mul
  | DIV -> build_udiv
  | EQ -> build_icmp Icmp.Eq
  | NEQ -> build_icmp Icmp.Ne
  | GT -> build_icmp Icmp.Sgt
  | GTE -> build_icmp Icmp.Sge
  | LT -> build_icmp Icmp.Slt
  | LTE -> build_icmp Icmp.Sle
  | AND -> build_and
  | OR -> build_or
  | CONCAT ->
    fun lhs rhs name builder ->
      let concat_fn = lookup_function_exception "hamsterml_concat" the_module in
      let fnty = function_type i64 [| i64; i64 |] in
      build_call fnty concat_fn [| lhs; rhs |] name builder
  | ID_EQ -> failwith "'==' operation is not expected to be implemented"
;;

let build_unary_operation = function
  | UMINUS -> build_neg
  | NOT -> build_not
  | UPLUS -> fun value _ _ -> value
;;

let rec codegen_immexpr env =
  let list_helper lst =
    let allocated_list =
      build_call
        (function_type i64 [||])
        (lookup_function_exception "hamsterml_alloc_list" the_module)
        [||]
        "hamsterml_allc_list_n"
        builder
    in
    let add = lookup_function_exception "hamsterml_add_to_list" the_module in
    let fnty = function_type i64 [| i64; i64 |] in
    Base.List.fold (Base.List.rev lst) ~init:allocated_list ~f:(fun acc elem ->
      let elem = codegen_immexpr env elem in
      let acc = acc in
      build_call fnty add [| acc; elem |] "hamsterml_add_to_list" builder)
  in
  function
  | ImmInt i -> const_int i64 i
  | ImmBool b -> const_int i64 (Base.Bool.to_int b)
  | ImmList l -> list_helper l
  | ImmTuple tpl ->
    let allocated_tuple =
      build_call
        (function_type i64 [| i64 |])
        (lookup_function_exception "hamsterml_alloc_tuple" the_module)
        [| Base.List.length tpl |> const_int i64 |]
        "hamsterml_alloc_tuple_n"
        builder
    in
    Base.List.fold tpl ~init:allocated_tuple ~f:(fun acc elem ->
      let elem = codegen_immexpr env elem in
      let acc = acc in
      build_call
        (function_type i64 [| i64; i64 |])
        (lookup_function_exception "hamsterml_fill_tuple" the_module)
        [| acc; elem |]
        "hamsterml_fill_tuple_n"
        builder)
  | ImmString s ->
    Base.List.init (Base.String.length s) ~f:(Base.String.get s)
    |> Base.List.map ~f:(fun c -> ImmInt (Base.Char.to_int c))
    |> list_helper
  | ImmId id ->
    (match Base.Map.find env id with
     | Some value ->
       let llval = value in
       build_load i64 llval id builder
     | None ->
       let llv = lookup_function_exception id the_module in
       if params llv |> Base.Array.length = 0
       then (
         let fnty = function_type i64 [| i64 |] in
         let func = lookup_function_exception "hamsterml_apply0" the_module in
         let args = [| build_pointercast llv i64 "ptr_to_i64_n" builder |] in
         build_call fnty func args "hamsterml_apply0_n" builder)
       else llv)
  | ImmOperation op ->
    (match op with
     | Binary bop -> codegen_immexpr env (ImmId (BinOperator.to_string bop))
     | Unary uop -> codegen_immexpr env (ImmId (UnOperator.to_string uop)))
  | ImmUnit -> const_int i64 0
;;

let rec codegen_cexpr env = function
  | CImm ie -> codegen_immexpr env ie
  | CConstructList (ie1, ie2) ->
    let arg_value = codegen_immexpr env ie1 in
    let arg_list = codegen_immexpr env ie2 in
    let add_to_list = lookup_function_exception "hamsterml_add_to_list" the_module in
    let fnty = function_type i64 [| i64; i64 |] in
    let result =
      build_call
        fnty
        add_to_list
        [| arg_list; arg_value |]
        "hamsterml_add_to_list_n"
        builder
    in
    result
  | CApplication (ce1, ce2, ce_list) ->
    let fun_name =
      match ce1 with
      | CImm (ImmId id) -> id
      | _ -> failwith "not a function"
    in
    let function_value =
      match Base.Map.find env fun_name with
      | Some value -> build_load i64 value fun_name builder
      | None -> lookup_function_exception fun_name the_module
    in
    let args = Base.List.map ~f:(codegen_cexpr env) (ce2 :: ce_list) in
    let args_arr = Array.of_list args in
    let arg_types = Array.make (Array.length args_arr) i64 in
    let fnty = function_type i64 arg_types in
    build_call fnty function_value args_arr "calltmp" builder
  | CIf (ie_condition, ae_then, ae_opt_else) ->
    let start_bb = insertion_block builder in
    let condition = codegen_immexpr env ie_condition in
    let the_function = block_parent (insertion_block builder) in
    let then_block = append_block global_context "then" the_function in
    let else_block = append_block global_context "else" the_function in
    let merge_block = append_block global_context "ifcont" the_function in
    position_at_end then_block builder;
    let then_value = codegen_aexpr env ae_then in
    let then_block_end = insertion_block builder in
    ignore (build_br merge_block builder);
    position_at_end else_block builder;
    let else_value =
      match ae_opt_else with
      | Some ae_else -> codegen_aexpr env ae_else
      | None -> const_int i64 0
    in
    let else_block_end = insertion_block builder in
    ignore (build_br merge_block builder);
    position_at_end start_bb builder;
    ignore (build_cond_br condition then_block else_block builder);
    position_at_end merge_block builder;
    let incoming = [ then_value, then_block_end; else_value, else_block_end ] in
    let phi = build_phi incoming "iftmp" builder in
    phi

and codegen_aexpr env = function
  | ACExpr cexpr -> codegen_cexpr env cexpr
  | ALetIn (_, cexpr, aexpr) ->
    let cexpr_value = codegen_cexpr env cexpr in
    let alloca = build_alloca i64 "_" builder in
    let _ = build_store cexpr_value alloca builder in
    let env = Base.Map.add_exn env ~key:"_" ~data:alloca in
    let aexpr_value = codegen_aexpr env aexpr in
    aexpr_value
;;

let codegen_single_anf_binding = function
  | ALet (fun_name, arg_list, ae) ->
    let arg_types = Array.of_list (Base.List.map arg_list ~f:(fun _ -> i64)) in
    let func_type = function_type i64 arg_types in
    let llvm_function = define_function fun_name func_type the_module in
    let entry_bb = append_block global_context "entry" llvm_function in
    position_at_end entry_bb builder;
    let fun_symtab =
      Base.Array.foldi
        (params llvm_function)
        ~init:(Base.Map.empty (module Base.String))
        ~f:(fun i table llvm_arg ->
          let arg_name = Base.List.nth_exn arg_list i in
          set_value_name arg_name llvm_arg;
          Base.Map.add_exn table ~key:arg_name ~data:llvm_arg)
    in
    let body_val = codegen_aexpr fun_symtab ae in
    let _ = build_ret body_val builder in
    llvm_function
;;

let codegen_anf_decl = function
  | ADSingleLet (_, single_anf_binding) -> codegen_single_anf_binding single_anf_binding
  | ADMutualRecDecl bindings ->
    let llvalues = Base.List.map bindings ~f:codegen_single_anf_binding in
    (match Base.List.last llvalues with
     | Some llvalue -> llvalue
     | None -> const_null i64)
;;

let stdlib = [ "print_int", 2 ]

let codegen program =
  let env =
    Base.List.map stdlib ~f:(fun (id, args_num) ->
      declare_function id (function_type i64 (Array.make args_num i64)) the_module)
  in
  let rec codegen acc env = function
    | [] -> acc
    | head :: tail ->
      let head = codegen_anf_decl head in
      codegen (head :: acc) env tail
  in
  let result = codegen env Base.Map.Poly.empty program in
  Base.List.rev result
;;

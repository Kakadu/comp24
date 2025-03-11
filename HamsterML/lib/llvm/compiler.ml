open Llvm
open Ast
open Anf

(* LLVM setup *)
let global_context = global_context ()
let builder = builder global_context
let the_module = create_module global_context "HamsterML_LLVM_Compiler"
let i64 = i64_type global_context
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
     | Some value -> build_load i64 value id builder
     | None ->
       (match lookup_function id the_module with
        | Some llv -> llv
        | None -> failwith ("Unknown identifier: " ^ id)))
  | ImmOperation op ->
    (match op with
     | Binary bop ->
       let op_name = BinOperator.to_string bop in
       (match lookup_function op_name the_module with
        | Some llv -> llv
        | None -> failwith ("Operator not pre-defined: " ^ op_name))
     | Unary uop -> codegen_immexpr env (ImmId (UnOperator.to_string uop)))
  | ImmUnit -> const_int i64 0

and codegen_cexpr env = function
  | CImm ie -> codegen_immexpr env ie
  | CConstructList (ie1, ie2) ->
    let arg_value = codegen_immexpr env ie1 in
    let arg_list = codegen_immexpr env ie2 in
    let add_to_list = lookup_function_exception "hamsterml_add_to_list" the_module in
    let fnty = function_type i64 [| i64; i64 |] in
    build_call
      fnty
      add_to_list
      [| arg_list; arg_value |]
      "hamsterml_add_to_list_n"
      builder
  | CApplication (CImm (ImmId fun_name), arg, rest_args) ->
    let function_value =
      match Base.Map.find env fun_name with
      | Some alloca -> build_load i64 alloca fun_name builder
      | None ->
        (match lookup_function fun_name the_module with
         | Some fn -> fn
         | None ->
           (match fun_name with
            | "+" | "-" | "*" | "/" | "<=" | ">=" | "<" | ">" | "=" | "!=" ->
              let op =
                match fun_name with
                | "+" -> ADD
                | "-" -> SUB
                | "*" -> MUL
                | "/" -> DIV
                | "<=" -> LTE
                | ">=" -> GTE
                | "<" -> LT
                | ">" -> GT
                | "=" -> EQ
                | "!=" -> NEQ
                | _ -> failwith ("Unsupported operation: " ^ fun_name)
              in
              lookup_function_exception (BinOperator.to_string op) the_module
            | _ -> failwith ("Unknown function: " ^ fun_name)))
    in
    let args = Base.List.map ~f:(codegen_cexpr env) (arg :: rest_args) in
    let args_arr = Array.of_list args in
    let arg_types = Array.make (Array.length args_arr) i64 in
    let fnty = function_type i64 arg_types in
    build_call fnty function_value args_arr "calltmp" builder
  | CApplication (func_expr, arg, rest_args) ->
    let function_value = codegen_cexpr env func_expr in
    let args = Base.List.map ~f:(codegen_cexpr env) (arg :: rest_args) in
    let args_arr = Array.of_list args in
    let arg_types = Array.make (Array.length args_arr) i64 in
    let fnty = function_type i64 arg_types in
    build_call fnty function_value args_arr "indirect_calltmp" builder
  | CIf (ie_condition, ae_then, ae_opt_else) ->
    let start_bb = insertion_block builder in
    let condition_i64 = codegen_immexpr env ie_condition in
    let condition = build_icmp Icmp.Ne condition_i64 (const_int i64 0) "ifcond" builder in
    let the_function = block_parent (insertion_block builder) in
    let then_block = append_block global_context "then" the_function in
    let else_block = append_block global_context "else" the_function in
    let merge_block = append_block global_context "ifcont" the_function in
    position_at_end start_bb builder;
    ignore (build_cond_br condition then_block else_block builder);
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
    position_at_end merge_block builder;
    let incoming = [ then_value, then_block_end; else_value, else_block_end ] in
    build_phi incoming "iftmp" builder

and codegen_aexpr env = function
  | ACExpr cexpr -> codegen_cexpr env cexpr
  | ALetIn (Var id, cexpr, aexpr) ->
    let cexpr_value = codegen_cexpr env cexpr in
    let alloca = build_alloca i64 id builder in
    ignore (build_store cexpr_value alloca builder);
    let new_env = Base.Map.set env ~key:id ~data:alloca in
    codegen_aexpr new_env aexpr
  | ALetIn (Const Unit, cexpr, aexpr) ->
    ignore (codegen_cexpr env cexpr);
    codegen_aexpr env aexpr
  | ALetIn (_, _, _) ->
    failwith "Only variable and unit patterns are supported in let bindings"
;;

let codegen_single_anf_binding = function
  | ALet (fun_name, arg_list, ae) ->
    let func_type =
      function_type i64 (Array.of_list (Base.List.map arg_list ~f:(fun _ -> i64)))
    in
    let llvm_function = declare_function fun_name func_type the_module in
    let entry_bb = append_block global_context "entry" llvm_function in
    position_at_end entry_bb builder;
    let fun_symtab =
      Base.Array.foldi
        (params llvm_function)
        ~init:(Base.Map.empty (module Base.String))
        ~f:(fun i table llvm_arg ->
          let arg_name = Base.List.nth_exn arg_list i in
          set_value_name arg_name llvm_arg;
          let alloca = build_alloca i64 arg_name builder in
          ignore (build_store llvm_arg alloca builder);
          Base.Map.set table ~key:arg_name ~data:alloca)
    in
    let body_val = codegen_aexpr fun_symtab ae in
    ignore (build_ret body_val builder);
    llvm_function
;;

let codegen_anf_decl = function
  | ADSingleLet (_, single_anf_binding) -> codegen_single_anf_binding single_anf_binding
  | ADMutualRecDecl bindings ->
    (* First declare all functions so they can reference each other *)
    let function_declarations =
      Base.List.map bindings ~f:(function ALet (name, args, _) ->
        let fn_type =
          function_type i64 (Array.of_list (Base.List.map args ~f:(fun _ -> i64)))
        in
        let fn = declare_function name fn_type the_module in
        set_linkage Linkage.Internal fn;
        name, args, fn)
    in
    (* Define all function bodies with access to all declarations *)
    Base.List.iter function_declarations ~f:(fun (name, args, fn) ->
      let binding =
        Base.List.find_exn bindings ~f:(function ALet (n, _, _) -> n = name)
      in
      let entry_bb = append_block global_context "entry" fn in
      position_at_end entry_bb builder;
      let fun_env =
        Base.Array.foldi
          (params fn)
          ~init:(Base.Map.empty (module Base.String))
          ~f:(fun i env llvm_arg ->
            let arg_name = Base.List.nth_exn args i in
            set_value_name arg_name llvm_arg;
            let alloca = build_alloca i64 arg_name builder in
            ignore (build_store llvm_arg alloca builder);
            Base.Map.set env ~key:arg_name ~data:alloca)
      in
      match binding with
      | ALet (_, _, body) ->
        let body_val = codegen_aexpr fun_env body in
        ignore (build_ret body_val builder));
    (* Return the last function, matching the behavior of single binding *)
    (match Base.List.last function_declarations with
     | Some (_, _, llvalue) -> llvalue
     | None -> const_null i64)
;;

let stdlib =
  [ "print_int", 1
  ; "+", 2
  ; "-", 2
  ; "*", 2
  ; "/", 2
  ; "<=", 2
  ; ">=", 2
  ; "<", 2
  ; ">", 2
  ; "=", 2
  ; "!=", 2
  ]
;;

let codegen program =
  (* Declare standard library functions *)
  let _ =
    Base.List.map stdlib ~f:(fun (id, args_num) ->
      declare_function id (function_type i64 (Array.make args_num i64)) the_module)
  in
  (* Define binary operation wrapper functions *)
  let define_binary_op op name =
    let fnty = function_type i64 [| i64; i64 |] in
    let fn = declare_function name fnty the_module in
    let entry = append_block global_context "entry" fn in
    position_at_end entry builder;
    let lhs = param fn 0 in
    let rhs = param fn 1 in
    let build_op = build_binary_operation op in
    let result = build_op lhs rhs "result" builder in
    let final_result =
      match op with
      | EQ | NEQ | GT | GTE | LT | LTE -> build_zext result i64 "extended_result" builder
      | _ -> result
    in
    ignore (build_ret final_result builder);
    fn
  in
  (* Create wrappers for binary operations *)
  let _ = define_binary_op ADD "+" in
  let _ = define_binary_op SUB "-" in
  let _ = define_binary_op MUL "*" in
  let _ = define_binary_op DIV "/" in
  let _ = define_binary_op LTE "<=" in
  let _ = define_binary_op GTE ">=" in
  let _ = define_binary_op LT "<" in
  let _ = define_binary_op GT ">" in
  let _ = define_binary_op EQ "=" in
  let _ = define_binary_op NEQ "!=" in
  (* Add a main function to make the program executable *)
  let define_main () =
    let main_type = function_type i64 [||] in
    let main_fn = declare_function "main" main_type the_module in
    let entry = append_block global_context "entry" main_fn in
    position_at_end entry builder;
    let main_ret_val =
      match lookup_function "main" the_module with
      | Some user_main when user_main != main_fn ->
        build_call (function_type i64 [||]) user_main [||] "main_result" builder
      | _ ->
        (match
           Base.List.find_map program ~f:(function
             | ADSingleLet (_, ALet (name, [], _)) -> lookup_function name the_module
             | _ -> None)
         with
         | Some user_main_fn ->
           build_call (function_type i64 [||]) user_main_fn [||] "main_result" builder
         | None -> const_int i64 0)
    in
    ignore (build_ret main_ret_val builder);
    main_fn
  in
  (* Generate code for program *)
  let rec codegen acc env = function
    | [] -> acc
    | head :: tail ->
      let head = codegen_anf_decl head in
      codegen (head :: acc) env tail
  in
  let result = codegen [] Base.Map.Poly.empty program in
  (* Define main function after processing all declarations *)
  let _ = define_main () in
  Base.List.rev result
;;

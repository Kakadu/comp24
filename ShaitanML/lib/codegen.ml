open Llvm
open Anf_ast
open Llvm_utils

let ctx = global_context ()
let module_ = create_module ctx "ShaitanML"
let builder = builder ctx
let target_triple = Llvm_target.Target.default_triple ()
let () = Llvm.set_target_triple target_triple module_
let i64_t = i64_type ctx

let compile_binop op x y =
  match op with
  | "( + )" -> build_add x y "add" builder
  | "( - )" -> build_sub x y "sub" builder
  | "( * )" -> build_mul x y "mul" builder
  | "( / )" -> build_sdiv x y "div" builder
  | _ -> failwith ("Invalid operator: " ^ op)
;;

let rec compile_immexpr = function
  | ImmInt i -> const_int i64_t i
  | ImmBool b -> const_int i64_t (if b then 1 else 0)
  | ImmUnit -> const_int i64_t 0
  | ImmVar name ->
    let rt_name = id_to_runtime_name name in
    (match Llvm.lookup_function rt_name module_ with
     | Some f ->
       let fptr = build_ptrtoint f i64_t "" builder in
       build_call
         (function_type i64_t [| i64_t; i64_t |])
         (Option.get @@ lookup_function "create_closure" module_)
         [| fptr; const_int i64_t (Array.length (params f)); const_int i64_t 0 |]
         "empty_closure"
         builder
     | None ->
       (match lookup_global rt_name module_ with
        | Some g -> g
        | None ->
          (match lookup_name rt_name with
           | Some v -> v
           | None -> failwith ("Unknown variable: " ^ name))))

and compile_cexp =
  let match_op = function
    | "( + )" | "( - )" | "( * )" | "( / )" -> true
    | _ -> false
  in
  function
  | CImm imm -> compile_immexpr imm
  | CEApply (op, [ ie1; ie2 ]) when match_op op ->
    let x = compile_immexpr ie1 in
    let y = compile_immexpr ie2 in
    compile_binop op x y
  | CEApply (f, args) ->
    let rev_values =
      List.fold_left
        (fun acc ie ->
          let v = compile_immexpr ie in
          v :: acc)
        []
        (ImmVar f :: args)
    in
    let values = List.rev rev_values in
    let f, args = List.hd values, List.tl values in
    build_call
      (var_arg_function_type i64_t [| i64_t; i64_t |])
      (Option.get (lookup_function "apply_args_to_closure" module_))
      (Array.of_list ([ f; const_int i64_t (List.length args) ] @ args))
      "applied_closure"
      builder
  | CEIf (i, t, e) ->
    let cond = compile_immexpr i in
    let cond_val = build_icmp Icmp.Ne cond (const_int i64_t 0) "cond_val" builder in
    let entry_bb = insertion_block builder in
    let parent = block_parent entry_bb in
    let then_bb = append_block ctx "then" parent in
    position_at_end then_bb builder;
    let then_ = compile_aexp t in
    let new_then_bb = insertion_block builder in
    let else_bb = append_block ctx "else" parent in
    position_at_end else_bb builder;
    let else_ = compile_aexp e in
    let new_else_bb = insertion_block builder in
    let merge_bb = append_block ctx "merge" parent in
    position_at_end merge_bb builder;
    let phi = build_phi [ then_, new_then_bb; else_, new_else_bb ] "phi" builder in
    position_at_end entry_bb builder;
    let (_ : llvalue) = build_cond_br cond_val then_bb else_bb builder in
    position_at_end new_then_bb builder;
    let (_ : llvalue) = build_br merge_bb builder in
    position_at_end new_else_bb builder;
    let (_ : llvalue) = build_br merge_bb builder in
    position_at_end merge_bb builder;
    phi

and compile_aexp = function
  | ACExpr c -> compile_cexp c
  | AELetIn (name, cexp, aexp) ->
    let v = compile_cexp cexp in
    add_sym name v;
    compile_aexp aexp
;;

let declare_func name args =
  let signature = function_type i64_t (Array.make (List.length args) i64_t) in
  let func = declare_function name signature module_ in
  func
;;

let compile_func_body id args aexp =
  let func = declare_func id args in
  List.iter
    (fun (name, value) ->
      set_value_name name value;
      add_sym name value)
    (Base.List.zip_exn args (Base.List.of_array (params func)));
  let bb = append_block ctx "entry" func in
  position_at_end bb builder;
  let body = compile_aexp aexp in
  let _ = build_ret body builder in
  func
;;

let compile_str_item = function
  | Value (name, aexp) ->
    let body = compile_aexp aexp in
    let gvar = define_global name (const_int i64_t 0) module_ in
    ignore (build_store body gvar builder)
  | Non_rec (id, args, aexp) -> ignore (compile_func_body id args aexp)
  | Rec funcs ->
    List.iter (fun (id, args, _) -> ignore (declare_func id args)) funcs;
    List.iter (fun (id, args, aexp) -> ignore (compile_func_body id args aexp)) funcs
;;

let init_runtime () =
  let runtime_ =
    [ "create_closure", function_type i64_t [| i64_t; i64_t; i64_t |]
    ; "apply_args_to_closure", var_arg_function_type i64_t [| i64_t; i64_t; i64_t |]
    ; "print_int", function_type i64_t [| i64_t |]
    ; "print_bool", function_type i64_t [| i64_t |]
    ; "add", function_type i64_t [| i64_t; i64_t |]
    ; "sub", function_type i64_t [| i64_t; i64_t |]
    ; "mul", function_type i64_t [| i64_t; i64_t |]
    ; "div", function_type i64_t [| i64_t; i64_t |]
    ; "leq", function_type i64_t [| i64_t; i64_t |]
    ; "less", function_type i64_t [| i64_t; i64_t |]
    ; "geq", function_type i64_t [| i64_t; i64_t |]
    ; "gre", function_type i64_t [| i64_t; i64_t |]
    ; "eq", function_type i64_t [| i64_t; i64_t |]
    ; "neq", function_type i64_t [| i64_t; i64_t |]
    ; "and", function_type i64_t [| i64_t; i64_t |]
    ; "or", function_type i64_t [| i64_t; i64_t |]
    ; "fail_match", function_type i64_t [| i64_t |]
    ]
  in
  List.iter (fun (name, ty) -> ignore (declare_function name ty module_)) runtime_
;;

let create_main s =
  let main = declare_function "main" (function_type i64_t [||]) module_ in
  let bb = append_block ctx "entry" main in
  init_runtime ();
  List.iter
    (fun str_item ->
      position_at_end bb builder;
      ignore (compile_str_item str_item))
    s;
  ignore (build_ret (const_int i64_t 0) builder)
;;

let compile_structure s =
  create_main s;
  print_module "out.ll" module_
;;

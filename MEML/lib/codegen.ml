open Llvm
open Anfast
open Ast
open Result
module VarSMap = Stdlib.Map.Make (String)

let set_empty = Base.Set.empty (module Base.String)
let context = global_context ()
let the_module = create_module context "MEML"
let builder = builder context
let i8_type = Llvm.i8_type context
let i32_type = Llvm.i32_type context
let i64_type = i64_type context
let bool_type = Llvm.i1_type context
let ptr_type = Llvm.pointer_type context
let void_type = Llvm.void_type context
let ( let* ) = bind

let runtime_func =
  [ "create_int_val", function_type ptr_type [| i32_type |]
  ; "create_bool_val", function_type ptr_type [| bool_type |]
  ; "mul", function_type ptr_type [| ptr_type; ptr_type |]
  ; "divv", function_type ptr_type [| ptr_type; ptr_type |]
  ; "plus", function_type ptr_type [| ptr_type; ptr_type |]
  ; "minus", function_type ptr_type [| ptr_type; ptr_type |]
  ; "eq", function_type ptr_type [| ptr_type; ptr_type |]
  ; "neq", function_type ptr_type [| ptr_type; ptr_type |]
  ; "less", function_type ptr_type [| ptr_type; ptr_type |]
  ; "leq", function_type ptr_type [| ptr_type; ptr_type |]
  ; "gre", function_type ptr_type [| ptr_type; ptr_type |]
  ; "greq", function_type ptr_type [| ptr_type; ptr_type |]
  ; "or", function_type ptr_type [| ptr_type; ptr_type |]
  ; "and", function_type ptr_type [| ptr_type; ptr_type |]
  ; "print_int", function_type void_type [| ptr_type |]
  ; "new_closure", function_type ptr_type [| ptr_type; i32_type |]
  ; "app_closure", var_arg_function_type ptr_type [| ptr_type; i32_type |]
  ; "call_closure", function_type ptr_type [| ptr_type |]
  ; "get_bool", function_type bool_type [| ptr_type |]
  ]
;;

let runtime =
  let varset = VarSMap.empty in
  let mel = VarSMap.add "( * )" "mul" varset in
  let div = VarSMap.add "( / )" "divv" mel in
  let add = VarSMap.add "( + )" "plus" div in
  let sub = VarSMap.add "( - )" "minus" add in
  let eq = VarSMap.add "( = )" "eq" sub in
  let neq = VarSMap.add "( <> )" "neq" eq in
  let less = VarSMap.add "( < )" "less" neq in
  let leq = VarSMap.add "( <= )" "leq" less in
  let gre = VarSMap.add "( > )" "gre" leq in
  let greq = VarSMap.add "( >= )" "greq" gre in
  let or_ = VarSMap.add "( || )" "or_" greq in
  let and_ = VarSMap.add "( & )" "and_" or_ in
  let print_int = VarSMap.add "print_int" "print_int" and_ in
  print_int
;;

let check_runtime_typ runtime_name =
  let res = List.find_opt (fun (cur_name, _) -> cur_name = runtime_name) runtime_func in
  match res with
  | Some (_, typ) -> ok (Some typ)
  | None -> error "Error: unbound runtime name"
;;

let aboba ext_env runtime_name =
  let llval = VarSMap.find_opt runtime_name ext_env in
  let* lltyp = check_runtime_typ runtime_name in
  match llval, lltyp with
  | None, _ | _, None -> error "Error: unbound runtime name"
  | Some llval, Some lltyp -> ok (llval, lltyp)
;;

let find_var_name_value env ext_env s =
  match VarSMap.find_opt s env with
  | Some var -> ok var
  | None ->
    (match VarSMap.find_opt s runtime with
     | Some runtime_name ->
       (match VarSMap.find_opt runtime_name ext_env with
        | Some func -> ok func
        | None -> error "Error: Unbound runtime name")
     | None -> error "Error: Unbound value")
;;

let clos ext_env id_val =
  match Llvm.classify_value id_val with
  | Llvm.ValueKind.Function ->
    let args_num = Array.length (params id_val) in
    let* func, typ = aboba ext_env "new_closure" in
    let closure =
      build_call
        typ
        func
        [| id_val; const_int i32_type args_num |]
        "empty_closure"
        builder
    in
    (match args_num with
     | 0 ->
       let* func, typ = aboba ext_env "call_closure" in
       ok @@ build_call typ func [| closure |] "call_closure_res" builder
     | _ -> ok closure)
  | _ -> ok id_val
;;

let find_clos name env ext_env =
  let* llval = find_var_name_value env ext_env name in
  clos ext_env llval
;;

let name_op = function
  | Add -> "( + )"
  | Sub -> "( - )"
  | Mul -> "( * )"
  | Div -> "( / )"
  | And -> "( & )"
  | Or -> "( || )"
  | Eq -> "( = )"
  | Neq -> "( <> )"
  | Less -> "( < )"
  | Gre -> "( > )"
  | Leq -> "( <= )"
  | Greq -> "( >= )"
;;

let name_gop = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | And -> "And"
  | Or -> "Or"
  | Eq -> "Eq"
  | Neq -> "Neq"
  | Less -> "Less"
  | Gre -> "Gre"
  | Leq -> "Leq"
  | Greq -> "Greq"
;;

let rec unroll_app acc = function
  | AApp (e1, e2) -> unroll_app (e2 :: acc) e1
  | e -> e :: acc
;;

let rec value_list acc = function
  | [] -> Ok (List.rev acc)
  | Ok x :: rest -> value_list (x :: acc) rest
  | Error e :: _ -> Error e
;;

let get_name = function
  | AVar a -> ok a
  | _ -> error "Invalid name"
;;

let rec codegen_aexpression gvars env ext_env = function
  | AVar "()" ->
    let* func, typ = aboba ext_env "create_bool_val" in
    ok @@ build_call typ func [| const_int bool_type 0 |] "bool_var" builder
  | AVar s ->
    let* id_val = find_clos s env ext_env in
    ok id_val
  | AConst (AInt n) ->
    let* func, typ = aboba ext_env "create_int_val" in
    ok @@ build_call typ func [| const_int i32_type n |] "int_var" builder
  | AConst (ABool b) ->
    let* func, typ = aboba ext_env "create_bool_val" in
    ok
    @@ build_call
         typ
         func
         [| const_int bool_type (if b then 1 else 0) |]
         "bool_var"
         builder
  | AApp (name, args) ->
    let name_args = unroll_app [] (AApp (name, args)) in
    let name, args = get_name @@ List.hd name_args, List.tl name_args in
    let* name = name in
    let* closure = find_clos name env ext_env in
    let rev_llvalues =
      List.fold_left
        (fun curr_lst im ->
           let new_llvalue = codegen_aexpression gvars env ext_env im in
           new_llvalue :: curr_lst)
        []
        args
    in
    let* llvalues = value_list [] @@ List.rev rev_llvalues in
    let* func, typ = aboba ext_env "app_closure" in
    let args_array =
      Array.of_list (closure :: const_int i32_type (List.length llvalues) :: llvalues)
    in
    ok @@ build_call typ func args_array "app_closure_res" builder
  | AIfElse (imm, aexpr1, aexpr2) ->
    let* cond_val = codegen_aexpression gvars env ext_env imm in
    let* func, typ = aboba ext_env "get_bool" in
    let cond_i1_val = build_call typ func [| cond_val |] "cond_i1" builder in
    let func = Llvm.block_parent (Llvm.insertion_block builder) in
    let then_block = Llvm.append_block context "then" func in
    let else_block = Llvm.append_block context "else" func in
    let merge_block = Llvm.append_block context "merge" func in
    ignore (Llvm.build_cond_br cond_i1_val then_block else_block builder);
    Llvm.position_at_end then_block builder;
    let* then_val = codegen_aexpression gvars env ext_env aexpr1 in
    ignore (Llvm.build_br merge_block builder);
    let then_block = insertion_block builder in
    Llvm.position_at_end else_block builder;
    let* else_val = codegen_aexpression gvars env ext_env aexpr2 in
    ignore (Llvm.build_br merge_block builder);
    let else_block = insertion_block builder in
    Llvm.position_at_end merge_block builder;
    let phi =
      Llvm.build_phi [ then_val, then_block; else_val, else_block ] "if_res" builder
    in
    ok phi
  | ABinOp (op, l, r) ->
    if Base.Set.mem gvars (name_gop op)
    then codegen_aexpression gvars env ext_env @@ AApp (AApp (AVar (name_gop op), l), r)
    else codegen_aexpression gvars env ext_env @@ AApp (AApp (AVar (name_op op), l), r)
  | AVars (l, r) ->
    let _ = codegen_aexpression gvars env ext_env l in
    codegen_aexpression gvars env ext_env r
  | _ -> error "Not implemented"
;;

let lets env = function
  | ALet (_, name, args, _) ->
    let func_type =
      match name with
      | "main" -> function_type i32_type (Array.make (List.length args) ptr_type)
      | _ -> function_type ptr_type (Array.make (List.length args) ptr_type)
    in
    let func = declare_function name func_type the_module in
    ok @@ VarSMap.add name func env
  | _ -> error "Not implemented"
;;

let codgen_let gvars env ext_env = function
  | ALet (_, name, args, aexpr) ->
    let func =
      match VarSMap.find_opt name env with
      | Some var -> var
      | None -> failwith "Error: Unbound value"
    in
    let entry = append_block context "entry" func in
    Llvm.position_at_end entry builder;
    let env =
      List.fold_left2
        (fun env arg param -> VarSMap.add arg param env)
        env
        args
        (Array.to_list (Llvm.params func))
    in
    let* ret = codegen_aexpression gvars env ext_env aexpr in
    ok
      ( (match name with
         | "main" -> ignore (Llvm.build_ret (const_int i32_type 0) builder)
         | _ -> ignore (Llvm.build_ret ret builder))
      , Base.Set.add gvars name )
  | _ -> error "Not implemented"
;;

let runtime =
  List.fold_left
    (fun ext_env (name, typ) ->
       let ext_func = Llvm.declare_function name typ the_module in
       VarSMap.add name ext_func ext_env)
    VarSMap.empty
    runtime_func
;;

let codegen_program program =
  let ext_env = runtime in
  let env =
    List.fold_left
      (fun acc let_ ->
         let* env = acc in
         let* lets = lets env let_ in
         Ok lets)
      (Ok VarSMap.empty)
      program
  in
  let result =
    Base.List.fold
      ~init:(Ok set_empty)
      ~f:(fun acc let_ ->
        let* env = env in
        let* gvars = acc in
        let* _, new_gvars = codgen_let gvars env ext_env let_ in
        Ok new_gvars)
      program
  in
  result
;;

let codegen program =
  match codegen_program program with
  | Ok _ -> Ok (string_of_llmodule the_module)
  | Error e -> Error e
;;

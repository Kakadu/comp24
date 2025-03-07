(** Copyright 2023-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)
open Llvm

open Anf.Anf_ast
open AstLib.Ast
open Llvm_utils
open R

let identifier_to_str = function
  | IdConstraint (s, _) -> s
  | Id s -> s
;;

let codegen_const = function
  | CInt x -> return @@ const_int i64 x
  | CBool x -> return @@ const_int i64 (Bool.to_int x)
  | CUnit -> return @@ const_int i64 0
  | _ -> fail "Not implemented"
;;

let rec codegen_imexpr = function
  | ImmConst c -> codegen_const c
  | ImmIdentifier id ->
    let name = Common.Ident_utils.ident_to_string id in
    let name = map_ident_to_runtime name in
    let* id_var = lookup_env_var name in
    (match id_var with
     | Some v -> return (build_load i64 v name builder)
     | None ->
       (match lookup_function name the_module with
        | Some func ->
          let int_ptr = Llvm.build_ptrtoint func i64 "" builder in
          return
            (build_call
               (function_type i64 [| i64; i64 |])
               (Option.get (lookup_function "create_closure" the_module))
               [| int_ptr
                ; params func |> Base.Array.length |> const_int i64
                ; const_int i64 0
               |]
               "created_empty_closure"
               builder)
        | None ->
          (match lookup_global name the_module with
           | Some value -> return value
           | _ -> fail @@ Format.sprintf "Unknown variable %s" name)))
  | ImmConstraint (immexpr, _) -> codegen_imexpr immexpr
;;

let optimize = function
  | "+" -> fun x y -> build_add x y "add" builder
  | "-" -> fun x y -> build_sub x y "sub" builder
  | "*" -> fun x y -> build_mul x y "mul" builder
  | "/" -> fun x y -> build_sdiv x y "div" builder
  | _ -> failwith "No optimization for this binop"
;;

let rec codegen_cexpr = function
  | CImmExpr e -> codegen_imexpr e
  | CApp (CApp (CImmExpr (ImmIdentifier bin_op), CImmExpr e1), CImmExpr e2)
    when Runtime.is_optimized_binop (Common.Ident_utils.ident_to_string bin_op) ->
    let* e1' = codegen_imexpr e1 in
    let* e2' = codegen_imexpr e2 in
    let binop = optimize (Common.Ident_utils.ident_to_string bin_op) in
    return @@ binop e1' e2'
  | CApp _ as capp ->
    let rec unroll_capp acc = function
      | CApp (e1, e2) -> unroll_capp (e2 :: acc) e1
      | e -> e :: acc
    in
    let* params = unroll_capp [] capp |> map codegen_cexpr in
    let callee, args = List.hd params, List.tl params in
    let args = [ callee; const_int i64 @@ List.length args ] @ args |> Array.of_list in
    return
    @@ build_call
         (var_arg_function_type i64 [| i64; i64 |])
         (Option.get (lookup_function "apply_args_to_closure" the_module))
         args
         "application_result"
         builder
  | CIf (cond, then_, else_) ->
    let* cond = codegen_imexpr cond in
    let cond_val = build_icmp Icmp.Ne cond (const_int i64 0) "ifcondition" builder in
    let start_bb = insertion_block builder in
    let the_function = block_parent start_bb in
    let then_bb = append_block context "then_br" the_function in
    position_at_end then_bb builder;
    let* then_val = codegen_aexpr then_ in
    let new_then_bb = insertion_block builder in
    let else_bb = append_block context "else_br" the_function in
    position_at_end else_bb builder;
    let* else_val = codegen_aexpr else_ in
    let new_else_bb = insertion_block builder in
    let merge_bb = append_block context "ifcontext" the_function in
    position_at_end merge_bb builder;
    let incoming = [ then_val, new_then_bb; else_val, new_else_bb ] in
    let phi = build_phi incoming "ifphi" builder in
    position_at_end start_bb builder;
    let (_ : llvalue) = build_cond_br cond_val then_bb else_bb builder in
    position_at_end new_then_bb builder;
    let (_ : llvalue) = build_br merge_bb builder in
    position_at_end new_else_bb builder;
    let (_ : llvalue) = build_br merge_bb builder in
    position_at_end merge_bb builder;
    return phi

and codegen_aexpr = function
  | ALetIn (id, cexpr, aexpr) ->
    let* body = codegen_cexpr cexpr in
    let alloca = build_alloca i64 (identifier_to_str id) builder in
    let (_ : llvalue) = build_store body alloca builder in
    let* _ = update_var (identifier_to_str id) alloca in
    codegen_aexpr aexpr
  | ACExpr cexpr -> codegen_cexpr cexpr
;;

let do_declare_function id args =
  let func_name = identifier_to_str id in
  let func_sig = function_type i64 (Array.make (List.length args) i64) in
  let func = declare_function func_name func_sig the_module in
  return func
;;

let codegen_let_body = function
  | id, [], aexpr ->
    let var_name = identifier_to_str id in
    let* body = codegen_aexpr aexpr in
    let var_global = define_global var_name (const_int i64 0) the_module in
    let (_ : llvalue) = build_store body var_global builder in
    return var_global
  | id, args, aexpr ->
    let* func = do_declare_function id args in
    let () =
      Array.iter
        (fun (name, value) -> set_value_name (identifier_to_str name) value)
        (Base.Array.zip_exn (Base.List.to_array args) (params func))
    in
    let basic_block = append_block context "entry" func in
    let () = position_at_end basic_block builder in
    let* _ =
      map
        (fun (name, value) ->
          let alloca = build_alloca i64 (identifier_to_str name) builder in
          let (_ : llvalue) = build_store value alloca builder in
          update_var (identifier_to_str name) alloca)
        (Base.List.zip_exn args (Array.to_list @@ params func))
    in
    let* ret_val = codegen_aexpr aexpr in
    let _ =
      if identifier_to_str id = "main"
      then build_ret (const_int i64 0) builder
      else build_ret ret_val builder
    in
    return func
;;

let codegen_bexpr = function
  | ADSingleLet (_, lb) ->
    let* lb = codegen_let_body lb in
    return [ lb ]
  | ADMutualRecDecl (_, lb1, lb2, lbs) ->
    let lbs = lb1 :: lb2 :: lbs in
    let _ = List.map (fun (id, args, _) -> do_declare_function id args) lbs in
    let* lb1 = codegen_let_body lb1 in
    let* lb2 = codegen_let_body lb2 in
    let* lbs = map codegen_let_body lbs in
    let lbs = lb1 :: lb2 :: lbs in
    return lbs
;;

let init_runtime =
  let get_typ typ =
    let rec helper = function
      (* NB: Runtime.runtime_members has no high order function *)
      | TArr (fst, snd) ->
        let* fst' = helper fst in
        let* snd' = helper snd in
        return (fst' @ snd')
      | (TGround _ | TVar _) as typ -> return [ typ ]
      | _ -> fail "not supported yet"
    in
    let* args = helper typ in
    match args with
    | [] -> fail "Unexpected empty typ"
    | _ :: [] -> fail "Unexpected const typ"
    | hd :: tl -> return (hd, List.rev tl)
  in
  let map_typ_to_llvm = function
    | TVar _ | TGround _ -> return i64
    | a ->
      fail
        (Format.asprintf "Llvm doesn't support types such as %a" AstLib.Pp_ast.pp_typ a)
  in
  map
    (fun (id, typ) ->
      let* ret_typ, arg_typ = get_typ typ in
      let* ret_typ' = map_typ_to_llvm ret_typ in
      let* arg_typ' = map map_typ_to_llvm arg_typ in
      let arg_typ_arr = Array.of_list arg_typ' in
      let f = declare_function id (function_type ret_typ' arg_typ_arr) the_module in
      return f)
    Runtime.runtime_members
;;

let codegen prog cont =
  let* env = init_runtime in
  let rec codegen acc = function
    | [] -> return acc
    | head :: tail ->
      let () = cont () in
      let* llvalue = codegen_bexpr head in
      let* _ = clean_env in
      codegen (llvalue @ acc) tail
  in
  let* result = codegen env prog in
  return result
;;

let create_main prog =
  let func_name = "main" in
  let func_sig = function_type i64 [||] in
  let main = declare_function func_name func_sig the_module in
  let basic_block = append_block context "entry" main in
  let cont () = position_at_end basic_block builder in
  let* other = codegen prog cont in
  let _ = build_ret (const_int i64 0) builder in
  return @@ (main :: other)
;;

let codegen s =
  let _, result = create_main s Env.empty in
  match result with
  | Ok _ ->
    let _ = print_module "out.ll" the_module in
    Ok ()
  | Error s -> Error s
;;

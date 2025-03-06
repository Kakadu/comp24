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
  | CUnit -> return @@ const_pointer_null void
  | _ -> fail "Not implemented"
;;

let rec codegen_imexpr = function
  | ImmConst c -> codegen_const c
  | ImmIdentifier id ->
    let name = Common.Ident_utils.ident_to_string id in
    let* id_var = lookup_env_var name in
    (match id_var with
     | Some v -> return (build_load2 i64 v name builder)
     | None ->
       (match lookup_function name the_module with
        | Some func ->
          let int_ptr = Llvm.build_ptrtoint func i64 "" builder in
          return
            (build_call2
               (function_type i64 [| i64; i64 |])
               (Option.get (lookup_function "create_closure" the_module))
               [| int_ptr
                ; params func |> Base.Array.length |> const_int i64
                ; const_int i64 0
               |]
               "created_empty_closure"
               builder)
        | None -> fail @@ Format.sprintf "Unknown variable %s" name))
  | ImmConstraint (immexpr, _) -> codegen_imexpr immexpr
;;

let is_binop = function
  | "+" | "-" | "*" | "/" | "=" | "!=" | "<" | "<=" | ">" | ">=" | "&&" | "||" -> true
  | _ -> false
;;

let codegen_binop = function
  | "+" -> fun x y -> build_add x y "add" builder
  | "-" -> fun x y -> build_sub x y "sub" builder
  | "*" -> fun x y -> build_mul x y "mul" builder
  | "/" -> fun x y -> build_sdiv x y "div" builder
  | "=" -> fun x y -> build_icmp Icmp.Eq x y "eq" builder
  | "!=" -> fun x y -> build_icmp Icmp.Ne x y "neq" builder
  | "<" -> fun x y -> build_icmp Icmp.Slt x y "less" builder
  | "<=" -> fun x y -> build_icmp Icmp.Sle x y "leq" builder
  | ">" -> fun x y -> build_icmp Icmp.Sgt x y "gre" builder
  | ">=" -> fun x y -> build_icmp Icmp.Sge x y "geq" builder
  | "&&" -> fun x y -> build_and x y "and" builder
  | "||" -> fun x y -> build_or x y "or" builder
  | _ -> failwith "Unknown binop"
;;

let rec codegen_cexpr = function
  | CImmExpr e -> codegen_imexpr e
  (* | CApp (CApp (CImmExpr e1, CImmExpr (ImmIdentifier bin_op)), CImmExpr e2)
    when is_binop (Common.Ident_utils.ident_to_string bin_op) ->
    let e1' = codegen_immexpr e1 in
    let e2' = codegen_immexpr e2 in
    let* arg = codegen_cexpr argument in
    return
    @@ build_call2
         (function_type i64 [| i64; i64; i64 |])
         (Option.get (lookup_function "apply_args_to_closure" the_module))
         [| calee; const_int i64 1; arg |]
         "application_result"
         builder *)
  | CApp (func, argument) ->
    let* calee = codegen_cexpr func in
    (*TODO ПЕРЕДЕЛАТЬ *)
    let* arg = codegen_cexpr argument in
    return
    @@ build_call2
         (function_type i64 [| i64; i64; i64 |])
         (Option.get (lookup_function "apply_args_to_closure" the_module))
         [| calee; const_int i64 1; arg |]
         "application_result"
         builder
  | CIf (cond, then_, else_) ->
    let* cond = codegen_imexpr cond in
    let cond = cond in
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

let codegen_bexpr = function
  | ADSingleLet (_, (id, args, aexpr)) ->
    let func_name = identifier_to_str id in
    let func_sig = function_type i64 (Array.make (List.length args) i64) in
    let func = declare_function func_name func_sig the_module in
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
  | _ -> fail ""
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

let codegen program =
  let* env = init_runtime in
  let* _ = clean_env in
  let rec codegen acc = function
    | [] -> return acc
    | head :: tail ->
      let* llvalue = codegen_bexpr head in
      let* _ = clean_env in
      codegen (llvalue :: acc) tail
  in
  let* result = codegen env program in
  return @@ Base.List.rev result
;;

let ( let+ ) x f =
  match x with
  | Error err -> Error err
  | Ok v -> f v
;;

let codegen s =
  let _, result = codegen s Env.empty in
  match result with
  | Ok _ ->
    let _ = print_module "out.ll" the_module in
    ()
  | Error s -> Format.printf "%s" s
;;

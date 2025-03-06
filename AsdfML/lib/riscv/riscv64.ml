(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Anf_ast
open Machine
open Emit
open Utils

let pp_env env =
  let open Format in
  Map.to_alist env
  |> List.map ~f:(fun (k, v) -> asprintf "%s -> %a" k pp_rvalue v)
  |> String.concat ~sep:", "
  |> sprintf "{%s}"
;;

let fn_args = ref (Map.empty (module String))

let pp_fn_args () =
  let open Format in
  Map.to_alist !fn_args
  |> List.map ~f:(fun (k, v) -> asprintf "%s -> %d" k v)
  |> String.concat ~sep:", "
  |> sprintf "{%s}"
;;

let counter = ref 0

let counter_next () =
  let n = !counter in
  counter := n + 1;
  n
;;

(* TODO:
   - gen_imm may rewrite a0 a1 a2
   - direct math should check if ops were redefined
   - save/restore sN regs
*)

let rec gen_tuple env xs =
  let tuple = emit_fn_call "ml_create_tuple" [ RInt (List.length xs) ] in
  let tuple = emit_store tuple ~comm:"tuple" in
  List.iteri xs ~f:(fun i x ->
    gen_imm env a2 x;
    let _ = emit_fn_call "ml_set_tuple_field" [ loc_to_rvalue tuple; RInt i; RReg a2 ] in
    ());
  tuple

and gen_imm (env : (id, loc, 'a) Map.t) dest = (* dbg "ENV: %s\n" (pp_env env); *)
  function
  | ImmInt n -> emit li dest n
  | ImmBool b -> emit li dest (if b then 1 else 0)
  | ImmId id ->
    let id = Std.lookup_extern id |> Option.value ~default:id in
    (match Map.find !fn_args id with
     | Some 0 ->
       (* Constants *)
       (match Map.find env id with
        | Some x -> emit_load_reg dest (loc_to_rvalue x) ~comm:id
        | None ->
          emit_load_reg dest (RFn id);
          emit_load_reg dest (ROffset (dest, 0)))
     | Some n_args ->
       emit comment (Format.sprintf "Creating closure for %s" id);
       let x = emit_fn_call "create_closure" [ RFn id; RInt n_args ] in
       emit_load_reg dest (RReg x) ~comm:id
     | None ->
       (match Map.find env id with
        | Some x -> emit_load_reg dest (loc_to_rvalue x) ~comm:id
        | None -> failwith (Format.sprintf "unbound id: %s" id)))
  | ImmUnit -> emit li dest 0
  | ImmNil ->
    let list = emit_fn_call "ml_create_list" [] in
    emit_load_reg dest (RReg list)
  | ImmTuple (x1, x2, xs) ->
    let xs = x1 :: x2 :: xs in
    let tuple = loc_to_rvalue (gen_tuple env xs) in
    emit_load_reg dest tuple
  | ImmList xs ->
    let list = emit_fn_call "ml_create_list" [] in
    let list = emit_store list ~comm:"list" in
    xs
    |> List.rev
    |> List.iter ~f:(fun x ->
      gen_imm env a0 x;
      let res = emit_fn_call "ml_list_cons" [ RReg a0; loc_to_rvalue list ] in
      emit_load list (RReg res));
    emit_load_reg dest (loc_to_rvalue list)

and gen_cexpr env dest = function
  | CApp (ImmId fn, args) when is_direct_binop fn && List.length args = 2 ->
    let fst = List.nth_exn args 0 in
    let snd = List.nth_exn args 1 in
    gen_imm env t0 fst;
    gen_imm env t1 snd;
    emit_direct_binop
      dest
      fn
      t0
      t1
      ~comm:(Format.asprintf "%a %s %a" pp_imm_expr fst fn pp_imm_expr snd)
  | CApp (ImmId fn, args) when is_direct_unop fn && List.length args = 1 ->
    let fst = List.nth_exn args 0 in
    gen_imm env t0 fst;
    emit_direct_unop dest fn t0 ~comm:(Format.asprintf "%s %a" fn pp_imm_expr fst)
  | CApp (fn, args) ->
    let is_rewrites_regs = function
      | ImmNil | ImmTuple _ | ImmList _ -> true
      | ImmId id ->
        (match Map.find !fn_args id with
         | Some _ -> true
         | None -> false)
      | _ -> false
    in
    let split_args args =
      let n_args = List.length args in
      let free_arg_regs = List.tl_exn arg_regs in
      match args with
      | args when n_args <= List.length free_arg_regs ->
        List.take free_arg_regs n_args |> List.zip_exn args, None
      | args ->
        List.split_n args (List.length free_arg_regs - 1)
        |> fun (in_regs, on_heap) ->
        List.zip_exn in_regs (List.drop_last_exn free_arg_regs), Some on_heap
    in
    let reg_args, heap_args = split_args args in
    let heap_args = Option.map heap_args ~f:(gen_tuple env) in
    (*  *)
    let rw_arg_locs =
      List.filter args ~f:is_rewrites_regs
      |> List.fold ~init:Map.Poly.empty ~f:(fun acc arg ->
        gen_imm env a0 arg;
        let loc = emit_store a0 in
        Map.set acc ~key:arg ~data:loc)
    in
    gen_imm env a0 fn;
    (*  *)
    reg_args
    |> List.iter ~f:(fun (arg, reg) ->
      if is_rewrites_regs arg
      then emit_load_reg reg (loc_to_rvalue (Map.find_exn rw_arg_locs arg))
      else gen_imm env reg arg);
    let _ =
      Option.map heap_args ~f:(fun tuple_loc ->
        emit_load_reg a7 (loc_to_rvalue tuple_loc))
    in
    let app_closure_fn =
      "apply_closure_"
      ^
      match heap_args with
      | Some _ -> "9plus"
      | None -> string_of_int (List.length reg_args)
    in
    let closure =
      emit_fn_call
        app_closure_fn
        (RReg a0 :: List.map reg_args ~f:(fun (_, reg) -> RReg reg))
    in
    emit_load_reg dest (RReg closure)
  | CIfElse (ImmBool true, then_, _) -> gen_aexpr env dest then_
  | CIfElse (ImmBool false, _, else_) -> gen_aexpr env dest else_
  | CIfElse (cond, then_, else_) ->
    gen_imm env t0 cond;
    let n = counter_next () in
    let label_else = Format.sprintf ".else_%d" n in
    let label_end = Format.sprintf ".end_%d" n in
    emit beq t0 zero label_else;
    gen_aexpr env dest then_;
    emit j label_end;
    emit label label_else;
    gen_aexpr env dest else_;
    emit label label_end
  | CImmExpr e -> gen_imm env dest e

and gen_aexpr env dest = function
  | ALet (id, cexpr, aexpr) ->
    gen_cexpr env a0 cexpr;
    let loc = emit_store a0 ~comm:id in
    let env = Map.set env ~key:id ~data:loc in
    gen_aexpr env dest aexpr
  | ACExpr cexpr -> gen_cexpr env dest cexpr

and gen_fn ?(data_sec = None) init_fns = function
  | Fn (id, [], _)
    when (not (String.equal "main" id))
         &&
         match Map.find !fn_args id with
         | None -> true
         | Some 0 -> true
         | _ -> false -> ()
  | Fn (id, args, aexpr) ->
    (* on-stack args + in-reg args + RA + FP + 1 word for last expr *)
    (* let stack_size = word * (3 + List.length args + Anf_ast.count_bindings fn) in *)
    (*  *)
    (* TODO: this *)
    set_fn_code ();
    let args_loc = dump_reg_args_to_stack args in
    let env =
      args_loc
      |> List.fold
           ~init:(Map.empty (module String))
           ~f:(fun env (arg, reg) -> Map.set env ~key:arg ~data:reg)
    in
    gen_aexpr env a0 aexpr;
    let stack_size =
      let sp = max (- !stack_pos - word) 0 in
      sp + (word * 3)
    in
    set_code ();
    emit_fn_decl id args stack_size;
    if String.equal id "main"
    then (
      emit call "runtime_init";
      List.iter init_fns ~f:(emit call));
    flush_fn ();
    (match data_sec with
     | Some id ->
       emit la t1 id;
       emit sd a0 (t1, 0)
     | None -> ());
    emit_fn_ret stack_size
;;

let gen_const_inits ?(print_anf = false) consts =
  List.fold consts ~init:[] ~f:(fun acc (id, init_id, exp) ->
    let fn = Fn (init_id, [ "_" ], exp) in
    if print_anf then Format.printf "%s ANF:@\n%a@\n" init_id pp_fn fn;
    gen_fn [] fn ~data_sec:(Some id);
    init_id :: acc)
  |> List.rev
;;

let gen_program ?(print_anf = false) consts (prog : Anf_ast.program) =
  let init_fns = gen_const_inits ~print_anf consts in
  List.iter prog ~f:(gen_fn init_fns)
;;

let init_env ast =
  let open Std in
  let env =
    List.fold
      ast
      ~init:(Map.empty (module String))
      ~f:(fun env fn ->
        match fn with
        | Fn (id, args, _) -> Map.set env ~key:id ~data:(List.length args))
  in
  let env =
    stdlib @ runtime
    |> List.fold ~init:env ~f:(fun env x ->
      Map.set env ~key:x.extern ~data:(Types.count_arrow_args x.typ))
  in
  env
;;

let peephole (code : (instr * string) Queue.t) =
  let rec helper = function
    | (i1, c1) :: (i2, c2) :: tl ->
      (match i1, i2 with
       | Sd (src1, dst1), Ld (dst2, src2)
         when equal_reg src1 dst2 && equal_offset dst1 src2 -> (i1, c1) :: helper tl
       | _ -> (i1, c1) :: helper ((i2, c2) :: tl))
    | (i, c) :: [] -> [ i, c ]
    | [] -> []
  in
  code |> Queue.to_list |> helper |> Queue.of_list
;;

let collect_consts ast =
  let helper consts = function
    | Fn (id, [], exp) when String.( <> ) "main" id -> (id, "init_" ^ id, exp) :: consts
    | _ -> consts
  in
  List.fold ast ~init:[] ~f:helper |> List.rev
;;

let emit_data_section consts =
  consts
  |> List.map ~f:(fun (id, _, _) -> Format.sprintf "%s: .dword 0" id)
  |> String.concat_lines
  |> Format.sprintf ".section .data\n%s"
  |> emit str
;;

let compile ?(out_file = "/tmp/out.s") ?(print_anf = false) (ast : Anf_ast.program) =
  let open Format in
  if print_anf then Format.printf "ANF:\n%a\n" Anf_ast.pp_program ast;
  Stdio.Out_channel.with_file out_file ~f:(fun out ->
    let fmt = formatter_of_out_channel out in
    fn_args := init_env ast;
    let consts = collect_consts ast in
    emit_data_section consts;
    emit str ".section .text";
    gen_program ~print_anf consts ast;
    let code = peephole code in
    Queue.iter
      ~f:(fun (i, comm) ->
        Format.fprintf
          fmt
          "%a%s"
          pp_instr
          i
          (if String.(comm <> "") then Format.sprintf "  # %s\n" comm else "\n"))
      code;
    ());
  Ok ()
;;

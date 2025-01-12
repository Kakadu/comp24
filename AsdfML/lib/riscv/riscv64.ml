open Base
open Anf_ast
open Machine
open Emit
open Utils

let pp_env env =
  let open Format in
  Map.to_alist env
  |> List.map ~f:(fun (k, v) -> asprintf "%s -> %a" k pp_reg v)
  |> String.concat ~sep:", "
  |> sprintf "{%s}"
;;

let pp_fn_args fn_args =
  let open Format in
  Map.to_alist fn_args
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

let rec gen_imm fn_args env dest = function
  | ImmInt n -> emit li dest n
  | ImmBool b -> emit li dest (if b then 1 else 0)
  | ImmId id ->
    (* dbg "FN_ARGS: %s\n" (pp_fn_args fn_args);
       dbg "ENV: %s\n" (pp_env env);
       dbg "lookup %s = %b\n" id (Map.find fn_args id |> Option.is_some); *)
    let id =
      match Std.lookup_extern id with
      | Some (fn, _) -> fn
      | None -> id
    in
    (match Map.find fn_args id with
     | Some n_args ->
       let id = String.substr_replace_all id ~pattern:"`" ~with_:"" in
       emit comment (Format.sprintf "Creating closure for %s" id);
       let x = emit_fn_call "create_closure" [ AsmFn id; AsmInt n_args ] in
       emit_load dest (AsmReg x)
     | None ->
       (match Map.find env id with
        | Some x -> emit_load dest (AsmReg x)
        | None -> failwith (Format.sprintf "unbound id: %s" id)))
  | ImmUnit -> failwith "todo: ImmUnit"
  | ImmNil -> failwith "todo: ImmNil"
  | ImmTuple xs ->
    let tuple = emit_fn_call "ml_create_tuple" [ AsmInt (List.length xs) ] in
    let tuple = emit_store tuple ~comm:"tuple" in
    List.iteri xs ~f:(fun i x ->
      gen_imm fn_args env a2 x;
      let res = emit_fn_call "ml_set_tuple_field" [ AsmReg tuple; AsmInt i; AsmReg a2 ] in
      emit_load_2 (AsmReg tuple) (AsmReg res));
    emit_load dest (AsmReg tuple)
  | ImmList xs ->
    failwith "todo: ImmList"

and gen_cexpr fn_args env dest = function
  (* TODO:
     CFApp/CApp with multiple arguments
     direct calls when possible
     direct math
  *)
  | CApp (fn, arg) ->
    gen_imm fn_args env a0 fn;
    gen_imm fn_args env a1 arg;
    let closure = emit_fn_call "apply_closure" [ AsmReg a0; AsmReg a1 ] in
    emit_load dest (AsmReg closure)
  | CIfElse (ImmBool true, then_, _) -> gen_aexpr fn_args env dest then_
  | CIfElse (ImmBool false, _, else_) -> gen_aexpr fn_args env dest else_
  | CIfElse (cond, then_, else_) ->
    gen_imm fn_args env t0 cond;
    let n = counter_next () in
    let label_else = Format.sprintf ".else_%d" n in
    let label_end = Format.sprintf ".end_%d" n in
    emit beq t0 zero label_else;
    gen_aexpr fn_args env dest then_;
    emit j label_end;
    emit label label_else;
    gen_aexpr fn_args env dest else_;
    emit label label_end
  | CImmExpr e -> gen_imm fn_args env dest e

and gen_aexpr fn_args env dest = function
  | ALet (id, cexpr, aexpr) ->
    gen_cexpr fn_args env a0 cexpr;
    let loc = emit_store a0 ~comm:id in
    let env = Map.set env ~key:id ~data:loc in
    gen_aexpr fn_args env dest aexpr
  | ACExpr cexpr -> gen_cexpr fn_args env a0 cexpr

and gen_fn fn_args = function
  | Fn (id, args, aexpr) as fn ->
    let id = String.substr_replace_all id ~pattern:"`" ~with_:"" in
    (* on-stack args + in-reg args + RA + FP + 1 word for last expr *)
    let stack_size = 8 * (3 + List.length args + Anf_ast.count_bindings fn) in
    (* dbg "FN %d: %a\n" n_bindings Anf_ast.pp_fn fn; *)
    let args_loc = emit_fn_decl id args stack_size in
    if String.equal id "main" then emit call "runtime_init";
    let env =
      args_loc
      |> List.fold
           ~init:(Map.empty (module String))
           ~f:(fun env (arg, reg) -> Map.set env ~key:arg ~data:reg)
    in
    gen_aexpr fn_args env a0 aexpr;
    emit_fn_ret stack_size
;;

let gen_program fn_args (prog : Anf_ast.program) = List.iter prog ~f:(gen_fn fn_args)

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
    |> List.fold ~init:env ~f:(fun env x -> Map.set env ~key:x.extern ~data:x.arity)
  in
  env
;;

let peephole (code : (instr * string) Queue.t) =
  let rec helper = function
    | [] -> []
    | (i, c) :: [] -> [ i, c ]
    | (i1, c1) :: (i2, c2) :: tl ->
      (match i1, i2 with
       | Sd (src1, dst1), Ld (dst2, src2) when equal_reg src1 dst2 && equal_reg dst1 src2
         -> (i1, c1) :: helper tl
       | _ -> (i1, c1) :: helper ((i2, c2) :: tl))
  in
  code |> Queue.to_list |> helper |> Queue.of_list
;;

let compile ?(out_file = "/tmp/out.s") (ast : Anf_ast.program) =
  let open Format in
  dbg "ANF: %a\n" pp_program ast;
  Stdio.Out_channel.with_file out_file ~f:(fun out ->
    let fmt = formatter_of_out_channel out in
    let fn_args = init_env ast in
    gen_program fn_args ast;
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

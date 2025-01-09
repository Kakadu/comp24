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
  | ImmTuple xs -> failwith "todo: ImmTuple"
  | ImmList xs -> failwith "todo: ImmList"

and gen_cexpr fn_args env dest = function
  (* TODO:
     CApp with multiple arguments
     direct calls when possible (anf?)
     IfElse with true/false
  *)
  | CApp (fn, arg) ->
    (* call create closure (needs fn ptr and number of args) *)
    (* call apply closure *)
    (* store closure in dest *)
    gen_imm fn_args env a0 fn;
    gen_imm fn_args env a1 arg;
    let closure = emit_fn_call "apply_closure" [ AsmReg a0; AsmReg a1 ] in
    emit_load dest (AsmReg closure)
  | CIfElse (cond, then_, else_) ->
    gen_imm fn_args env t0 cond;
    let label_else = Format.sprintf ".else_%d" (counter_next ()) in
    let label_end = Format.sprintf ".end_%d" (counter_next ()) in
    emit beq t0 zero label_else;
    gen_aexpr fn_args env dest then_;
    emit j label_end;
    emit label label_else;
    gen_aexpr fn_args env dest else_;
    emit label label_end;
  | CImmExpr e -> gen_imm fn_args env dest e

and gen_aexpr fn_args env dest = function
  | ALet (id, cexpr, aexpr) ->
    (* calc cexpr *)
    (* save res on stack *)
    (* update env *)
    (* calc aexpr to dest *)
    gen_cexpr fn_args env a0 cexpr;
    let loc = emit_store a0 in
    let env = Map.set env ~key:id ~data:loc in
    gen_aexpr fn_args env dest aexpr
  | ACExpr cexpr -> gen_cexpr fn_args env a0 cexpr

and gen_fn fn_args = function
  | Fn (id, args, aexpr) ->
    let id = String.substr_replace_all id ~pattern:"`" ~with_:"" in
    let args_loc = emit_fn_decl id args in
    if String.equal id "main" then emit call "runtime_init";
    let env =
      args_loc
      |> List.fold
           ~init:(Map.empty (module String))
           ~f:(fun env (arg, reg) -> Map.set env ~key:arg ~data:reg)
    in
    (* add args to env (for stack ones emit_fn_decl should return list of offsets?) *)
    gen_aexpr fn_args env a0 aexpr;
    emit_fn_ret ()
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

let compile ?(out_file = "/tmp/out.s") (ast : Anf_ast.program) =
  let open Format in
  (* dbg "ANF: %a\n" pp_program ast; *)
  Stdio.Out_channel.with_file out_file ~f:(fun out ->
    let fmt = formatter_of_out_channel out in
    let fn_args = init_env ast in
    gen_program fn_args ast;
    Queue.iter ~f:(fprintf fmt "%s") code;
    ());
  Ok ()
;;
(*
   let rec gen_imm env dest = function
   | ImmInt n -> emit li dest n
   | ImmBool b -> emit li dest (if b then 1 else 0)
   | ImmId id ->
   (match Hashtbl.find env id with
   | Some offset -> emit ld dest offset
   | None -> failwith (Format.sprintf "unbound id: %s" id))
   | ImmUnit -> failwith "todo: ImmUnit"
   | ImmNil -> failwith "todo: ImmNil"
   | ImmTuple xs -> failwith "todo: ImmTuple"
   | ImmList xs -> failwith "todo: ImmList"

   and gen_cexpr env dest = function
   (* TODO:
   CApp with multiple arguments
   direct calls when possible (anf?)
   *)
   | CApp (fn, arg) ->
   dbg "env: %s\n" (pp_env env);
   dbg "lookup %s\n" fn;
   let fn, n_args = Std.lookup_extern fn |> Option.value_exn in
   (* let closure = emit_fn_call "create_closure" [ AsmFn fn; AsmInt n_args ] in
   gen_imm env a1 arg;
   let closure = emit_fn_call "apply_closure" [ AsmReg closure; AsmReg a1 ] in
   emit ld dest closure; *)
   (*
   emit la a0 fn;
   emit li a1 n_args;
   emit call "create_closure";
   gen_imm env a1 arg;
   emit call "apply_closure";
   *)
   let closure = emit_fn_call "create_closure" [ AsmFn fn; AsmInt n_args ] in
   gen_imm env a1 arg;
   let closure = emit_fn_call "apply_closure" [ AsmReg closure; AsmReg a1 ] in
   (* todo:store closure *)
   ()
   | CIfElse (c, t, e) -> failwith "todo: CIfElse"
   | CImmExpr e -> gen_imm env dest e
   | _ -> failwith "todo: gen_cexpr"

   and gen_aexpr env dest = function
   | ALet (id, cexpr, aexpr) ->
   gen_cexpr env a0 cexpr;
   let offset = emit_store a0 in
   Hashtbl.set env ~key:id ~data:offset;
   gen_aexpr env dest aexpr
   | ACExpr cexpr -> gen_cexpr env a0 cexpr

   and gen_fn env = function
   | Fn (id, args, aexpr) ->
   emit_fn_decl id args;
   gen_aexpr env a0 aexpr;
   emit_fn_ret ()
   ;;

   let gen_program (prog : Anf_ast.program) =
   let (env : (id, reg) Base.Hashtbl.t) = Hashtbl.create (module String) in
   List.iter prog ~f:(gen_fn env)
   ;;
*)

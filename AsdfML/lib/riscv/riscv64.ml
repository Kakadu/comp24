open Base
open Anf_ast
open Machine
open Emit
open Utils

let pp_env env = 
  let open Format in
  Hashtbl.to_alist env |> List.map ~f:(fun (k, v) -> asprintf "%s -> %a" k pp_reg v) |> String.concat ~sep:", " |> sprintf "{%s}"

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
  | CApp (ImmId fn, arg) ->
    gen_imm env a0 arg;
    let fn = Std.lookup_extern fn |> Option.value ~default:fn in
    emit call fn
  | CIfElse (c, t, e) -> failwith "todo: CIfElse"
  | CImmExpr e -> gen_imm env dest e
  | _ -> failwith "todo: gen_cexpr"

and gen_aexpr env dest = function
  | ALet (id, cexpr, aexpr) ->
    gen_cexpr env a0 cexpr;
    let offset = emit_store a0 in
    Hashtbl.set env ~key:id ~data:offset;
    (* dbg "ENV: %s" (pp_env env); *)
    gen_aexpr env dest aexpr
  | ACExpr cexpr -> gen_cexpr env a0 cexpr

and gen_fn env = function
  | Fn (id, args, aexpr) ->
    emit_fn_decl id args;
    gen_aexpr env a0 aexpr;
    emit_fn_ret ()
;;

let codegen (prog : Anf_ast.program) = 
  let (env:(id, reg) Base.Hashtbl.t) = Hashtbl.create (module String) in
  List.iter prog ~f:(gen_fn env)

let compile ?(out_file = "/tmp/out.s") (ast : Anf_ast.program) =
  let open Format in
  (* Format.printf "ANF: %a\n" pp_program ast; *)
  Stdio.Out_channel.with_file out_file ~f:(fun out ->
    let fmt = formatter_of_out_channel out in
    codegen ast;
    Queue.iter ~f:(fprintf fmt "%s") code;
    ());
  Ok ()
;;

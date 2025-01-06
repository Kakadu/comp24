open Base
open Anf_ast
open Rv64
open Utils

let rec gen_imm dest = function
  | ImmInt n -> emit li dest n
  | ImmBool b -> emit li dest (if b then 1 else 0)
  | ImmId id -> 
    emit li dest 0
    (* failwith "todo: ImmId" *)
  | ImmUnit -> failwith "todo: ImmUnit"
  | ImmNil -> failwith "todo: ImmNil"
  | ImmTuple xs -> failwith "todo: ImmTuple"
  | ImmList xs -> failwith "todo: ImmList"

and gen_cexpr dest = function
  | CApp (ImmId fn, arg) ->
    gen_imm a0 arg;
    (* emit call ("ml_" ^ fn) *)
    emit call fn
  | CIfElse (c, t, e) -> failwith "todo: CIfElse"
  | CImmExpr e -> gen_imm dest e
  | _ -> failwith "todo: gen_cexpr"

and gen_aexpr dest = function
  | ALet (id, cexpr, aexpr) ->
    gen_cexpr a0 cexpr;
    gen_aexpr dest aexpr
  | ACExpr cexpr -> gen_cexpr a0 cexpr

and gen_fn = function
  | Fn (id, args, aexpr) ->
    (* let dealloc, locals = allocate_locals aexpr in *)
    (* dbg "N LOC: %d\n" locals; *)
    (* dbg "LOCALS: %s\n"(Addr_of_local.store |> Hashtbl.to_alist |> List.map ~f:(fun (a,b) -> a ^ "->" ^ (string_of_int b)) |> String.concat ~sep:" "); *)
    emit_fn_decl id args;
    gen_aexpr a0 aexpr;
    emit_fn_ret ();
;;

let codegen (prog : Anf_ast.program) = List.iter prog ~f:gen_fn

let compile ?(out_file = "/tmp/out.s") (ast : Anf_ast.program) =
  let open Format in
  (* Format.printf "ANF: %a\n" pp_program ast; *)
  Stdio.Out_channel.with_file out_file ~f:(fun out ->
    let fmt = formatter_of_out_channel out in
    (* fprintf fmt "%s" extern; *)
    codegen ast;
    Queue.iter ~f:(fprintf fmt "%s") code;
    ());
  Ok ()
;;

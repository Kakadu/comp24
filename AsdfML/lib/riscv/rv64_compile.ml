open Base
open Anf_ast
open Rv64

let rec gen_imm dest = function
  | ImmInt n -> emit li dest n
  | ImmBool b -> failwith "todo"
  | ImmId id -> failwith "todo"
  | ImmUnit -> failwith "todo"
  | ImmNil -> failwith "todo"
  | ImmTuple xs -> failwith "todo"
  | ImmList xs -> failwith "todo"

and gen_cexpr dest = function
  | CApp (fn, arg) -> failwith "todo"
  | CIfElse (c, t, e) -> failwith "todo"
  | CImmExpr e -> gen_imm dest e

and gen_aexpr = function
  | ALet (id, cexpr, aexpr) -> failwith "todo"
  | ACExpr cexpr -> gen_cexpr a0 cexpr

and gen_fn = function
  | Fn (id, args, aexpr) ->
    emit_fn_decl id;
    gen_aexpr aexpr;
    emit ret
;;

let codegen (prog : Anf_ast.program) = List.iter prog ~f:gen_fn

let compile ?(out_file = "/tmp/out.s") (ast : Anf_ast.program) =
  let open Format in
  Stdio.Out_channel.with_file out_file ~f:(fun out ->
    let fmt = formatter_of_out_channel out in
    fprintf fmt "%s" extern;
    codegen ast;
    Queue.iter ~f:(fprintf fmt "%s") code;
    ());
  Ok ()
;;

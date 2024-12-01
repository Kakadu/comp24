open Base

let extern =
  let open Std in
  List.map stdlib ~f:(fun x -> Format.sprintf "    .extern %s" x.extern)
  |> String.concat_lines
;;

let section_data = {|
    .section .data
|}

let codegen (prog : Cf_ast.program) =
  let open Cf_ast in
  let gen_def = function
    | CFDLet (id, expr) -> failwith "codegen: unsupported definition"
  in
  let gen_expr = function
    | CFConst c -> failwith "codegen: unsupported const"
    | CFVar v -> failwith "codegen: unsupported var"
    | CFApp (f, arg) -> failwith "codegen: unsupported app"
    | CFIfElse (cond, if_expr, else_expr) -> failwith "codegen: unsupported if-else"
    | CFFun (args, body) -> failwith "codegen: unsupported fun"
    | CFLetIn (def, exp) -> failwith "codegen: unsupported let-in"
    | _ -> failwith "codegen: unsupported match"
  in
  List.map prog ~f:gen_def
;;

let compile () =
  let open Format in
  Stdio.Out_channel.with_file "/tmp/out.s" ~f:(fun out ->
    let fmt = Format.formatter_of_out_channel out in
    let print = Format.fprintf fmt "%s" in
    ());
  Ok ()
;;

compile ()

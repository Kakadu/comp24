open Shaitanml_lib

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  let parse_ s =
    match Parser.parse s with
    | Ok parsed as ast ->
      (match Infer.run_infer parsed with
       | Ok _ -> ast
       | Error e -> Error (Format.asprintf "Infer error: %a\n" Infer.pp_error e))
    | Error e -> Error (Format.sprintf "Parsing error: %s\n" e)
  in
  match parse_ s with
  | Ok ast ->
    let nh, names_count, ast = Pat_elim.run_pe ast in
    let _, _, ast = Alpha.run_ac nh names_count ast in
    let ast_ = Closure.run_cc ast in
    Format.printf "%a" Pat_elim_ast.pp_pe_structure ast_
  | Error message -> Format.printf "%s" message
;;
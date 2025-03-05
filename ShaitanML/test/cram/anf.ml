open Shaitanml_lib

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  let parse_and_infer s =
    match Parser.parse s with
    | Ok parsed as ast ->
      (match Infer.run_infer parsed with
       | Ok _ -> ast
       | Error e -> Error (Format.asprintf "Infer error: %a\n" Infer.pp_error e))
    | Error e -> Error (Format.sprintf "Parsing error: %s\n" e)
  in
  match parse_and_infer s with
  | Ok ast ->
    let bindings, count, ast = Pat_elim.run_pe ast in
    let bindings, count, ast = Alpha.run_ac bindings count ast in
    let ast = Closure.run_cc ast in
    let bindings, count, ast = L_lifting.run_ll bindings count ast in
    let _, _, ast = Anf.run_anf bindings count ast in
    Format.printf "%a" Anf_ast.pp_anf_structure ast
  | Error message -> Format.printf "%s" message
;;

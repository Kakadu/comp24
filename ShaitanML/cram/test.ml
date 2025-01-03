open Shaitanml_lib

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match Parser.parse s with
  | Ok ast -> Format.printf "%a\n" Ast.pp_structure ast
  | Error message -> Format.printf "Error: %s\n" message
;;

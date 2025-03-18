let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match SlarnML_lib.Parser.parser s with
  | Ok ast -> print_string @@ SlarnML_lib.Pprint_ast.pp_exprs ast
  | Error message -> Format.printf "Error: %s\n" message
;;

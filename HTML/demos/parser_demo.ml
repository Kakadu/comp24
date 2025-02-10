let parse_and_print s =
  match Parser.parse_program s with
  | Ok actual -> Format.printf "%a\n" AstLib.Pp_ast.pp_prog actual
  | Error err -> Format.printf "%s\n" err
;;

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  parse_and_print s
;;

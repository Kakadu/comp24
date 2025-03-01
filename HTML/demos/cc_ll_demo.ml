let cc_ll_test s =
  match Parser.parse_program s with
  | Ok actual ->
    let prog = Anf.Cc_ll.closure_convert actual in
    (match prog with
     | Ok actual -> Format.printf "%a\n" AstLib.Pp_ast.pp_prog actual
     | Error err -> Format.printf "%s\n" err)
  | Error err -> Format.printf "%s\n" err
;;

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  cc_ll_test s
;;

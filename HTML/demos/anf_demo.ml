let anf_demo s =
  match Parser.parse_program s with
  | Ok actual ->
    let prog = Anf.Cc_ll.closure_convert actual in
    let prog = Anf.Anf_conv.anf_program prog in
    Format.printf "%a\n" Anf.Pp_anf_ast.pp_anf_prog prog
  | Error err -> Format.printf "%s\n" err
;;

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  anf_demo s
;;

let anf_demo s =
  match Parser.parse_program s with
  | Ok actual ->
    let prog = Anf.Cc_ll.closure_convert actual in
    (match prog with
     | Ok actual ->
       let prog = Patelim.Elim.p_elim_decls actual in
       (match prog with
        | Ok actual ->
          let prog = Anf.Anf_conv.run actual in
          (match prog with
           | Ok actual -> Format.printf "%a\n" Anf.Pp_anf_ast.pp_anf_prog actual
           | Error err -> Format.printf "%s\n" err)
        | Error err -> Format.printf "%s\n" err)
     | Error err -> Format.printf "%s\n" err)
  | Error err -> Format.printf "%s\n" err
;;

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  anf_demo s
;;

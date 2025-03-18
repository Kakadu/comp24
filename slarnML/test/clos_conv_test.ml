let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  let result =
    match SlarnML_lib.Parser.parser s with
    | Ok ast -> SlarnML_lib.Clos_conv.clos_conv ast
    | Error message -> SlarnML_lib.Res.Error message
  in
  match result with
  | SlarnML_lib.Res.Result r ->
    print_string @@ String.concat "\n" (List.map SlarnML_lib.Pprint_cc.pp_cc_expr r)
  | Error e -> Printf.eprintf "%s" e
;;

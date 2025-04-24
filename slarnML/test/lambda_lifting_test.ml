open SlarnML_lib.Res

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  let result =
    match SlarnML_lib.Parser.parser s with
    | Ok ast ->
      SlarnML_lib.Clos_conv.clos_conv ast
      >>= fun ast -> SlarnML_lib.Lambda_lifting.lambda_lifting ast
    | Error message -> SlarnML_lib.Res.Error message
  in
  match result with
  | SlarnML_lib.Res.Result r ->
    print_string @@ String.concat "\n" (List.map SlarnML_lib.Pprint_ll.pp_gl_expr r)
  | Error e -> Printf.eprintf "%s" e
;;

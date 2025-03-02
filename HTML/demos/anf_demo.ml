let ( let+ ) x f =
  match x with
  | Error err -> Error err
  | Ok v -> f v
;;

let anf_demo s =
  let open TypeInference in
  let open Result in
  let+ prog = Parser.parse_program s in
  Format.printf "Type inference before:\n";
  Inferencer.run_inference prog |> Inferencer.print_env;
  Format.printf "\n";
  let+ prog = Patelim.Elim.p_elim_decls prog in
  let+ prog = Anf.Cc_ll.closure_convert prog in
  let+ prog = Anf.Anf_conv.run prog in
  let restored_ast = Anf.Restore_ast.convert_anf_prog prog in
  Format.printf "Type inference after:\n";
  let env = Inferencer.run_inference restored_ast in
  match env with
  | Error err ->
    let err = Format.asprintf "Typecheck error: %a" Typing.pp_error err in
    Error err
  | Ok _ ->
    Inferencer.print_env env;
    Format.printf "\n";
    Format.printf "%a\n" Anf.Pp_anf_ast.pp_anf_prog prog;
    Ok ()
;;

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match anf_demo s with
  | Ok _ -> ()
  | Error err -> Format.printf "%s\n" err
;;

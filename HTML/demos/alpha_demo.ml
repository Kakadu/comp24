let ( let+ ) x f =
  match x with
  | Error err -> Error err
  | Ok v -> f v
;;

let alpha_demo s =
  let+ actual = Parser.parse_program s in
  let+ actual_cc = Anf.Cc_ll.closure_convert actual in
  let+ actual_alpha = Anf.Alpha_conve.alpha_convert_prog actual_cc in
  Format.printf
    "---СС---\n\n%a\n\n---Alpha conv.---\n\n%a\n"
    AstLib.Pp_ast.pp_prog
    actual_cc
    AstLib.Pp_ast.pp_prog
    actual_alpha;
  Ok ()
;;

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match alpha_demo s with
  | Ok _ -> ()
  | Error err -> Format.printf "%s\n" err
;;

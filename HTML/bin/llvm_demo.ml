let ( let+ ) x f =
  match x with
  | Error err -> Error err
  | Ok v -> f v
;;

let llvm_test s =
  let+ actual = Parser.parse_program s in
  let+ actual_cc = Anf.Cc_ll.closure_convert actual in
  let+ actual_alpha_conv = Anf.Alpha_conve.alpha_convert_prog actual_cc in
  let+ actual_pe = Patelim.Elim.p_elim_decls actual_alpha_conv in
  let+ actual_cc = Anf.Cc_ll.closure_convert actual_pe in
  let+ actual_anf = Anf.Anf_conv.run actual_cc in
  let+ _ = Llvm_codegen.Codegen.codegen actual_anf in
  Ok ()
;;

let () =
  let s = Stdio.In_channel.input_all Stdlib.stdin in
  match llvm_test s with
  | Ok _ -> ()
  | Error err -> Format.printf "Error: %s\n" err
;;

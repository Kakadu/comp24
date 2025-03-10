open HamsterML.Ast
open HamsterTest.AlphaConversionTest
open HamsterML.Typing

let infer_prog_raw (prog : prog) =
  let inf_res = R.run (Infer.infer_prog TypeEnv.default prog) in
  match inf_res with
  | Ok env -> TypeEnv.pp Format.std_formatter env
  | Error e -> failwith (show_error e)
;;

let () =
  let s = In_channel.input_all Stdlib.stdin in
  let prog = alpha_conv_prog s in
  infer_prog_raw prog
;;

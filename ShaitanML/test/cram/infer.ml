open Shaitanml_lib.Infer

let () =
  let str = In_channel.input_all stdin in
  test_infer str
;;

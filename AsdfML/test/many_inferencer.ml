open Test.Utils

let () =
  let s = In_channel.input_all Stdlib.stdin in
  test_inferencer s
;;

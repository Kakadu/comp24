open HamsterTest.TypingTest

let () =
  let s = In_channel.input_all Stdlib.stdin in
  infer_prog s
;;

open HamsterTest.ParserTest

let () =
  let s = In_channel.input_all Stdlib.stdin in
  match parse_prog s with
  | _ -> ()
;;

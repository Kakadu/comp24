open HamsterML.Ast
open HamsterTest.ParserTest

let () =
  let s = In_channel.input_all Stdlib.stdin in
  let prog = parse_prog s in
  pp_prog Format.std_formatter prog
;;

open HamsterML.Compiler
open HamsterTest.AnfTest

let compile_program str =
  let anf = anf_prog str in
  let _ = codegen anf in
  Llvm.print_module "-" the_module
;;

let () =
  let s = In_channel.input_all Stdlib.stdin in
  compile_program s
;;

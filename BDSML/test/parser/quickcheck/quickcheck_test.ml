open Quickcheck
open Parser

let make_ast =
  QCheck.make
    (QCheck.Gen.sized Generator.gen_structure)
    ~print:(fun structure -> Main_unparser.unparse_structure structure)
    ~shrink:Shrinker.shrink_structure
;;

let test =
  QCheck.(
    Test.make ~count:100 make_ast (fun input ->
      let str = Main_unparser.unparse_structure input in
      let res = Parser.Main_parser.parse str in
      match res with
      | Result.Ok output when output = input -> true
      | Result.Ok output ->
        Format.printf "\n\n!!!AST DIFFERS!!!\n\n";
        Format.printf "### OUTPUT AST ### \n%s\n########\n\n" (Ast.show_structure output);
        Format.printf "### INPUT AST ### \n%s\n########\n\n" (Ast.show_structure input);
        false
      | Result.Error s ->
        Format.printf "\n\n[!]: Parser error: %s\n" s;
        Format.printf "On test:\n %s\n" str;
        false))
;;

let%expect_test "QuickCheck round-trip test for declarations (depth ≤ 3, no shrinker)" =
  QCheck_runner.set_seed 52;
  let _ = QCheck_runner.run_tests ~colors:false [ test ] in
  ();
  [%expect {||}]
;;

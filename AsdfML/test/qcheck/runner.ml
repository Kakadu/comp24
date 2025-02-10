open Shrinker
open QCheck
open Lib

let parser_qtests =
  [ QCheck.Test.make ~count:10 arbitrary_ast (fun original ->
      let src = Format.asprintf "%a" Pp_ast.pp_program original in
      let res = Parser.parse_program src in
      match res with
      | Result.Ok parsed when parsed = original -> true
      | Result.Ok parsed ->
        Format.printf "\n\nDifferent AST!\n";
        let print pp o p =
          Format.printf "Original AST \n%a \n\nParsed AST \n%a\n\n" pp o pp p
        in
        print Pp_ast.pp_program original parsed;
        print Ast.pp_program original parsed;
        false
      | Result.Error s ->
        Format.printf "\n\nParser error: %s\n" s;
        false)
  ]
;;

let%expect_test _ =
  QCheck_runner.set_seed 42;
  let _ = QCheck_runner.run_tests ~colors:false parser_qtests in
  ();
  [%expect
    {|
    random seed: 42
    ================================================================================
    success (ran 1 tests)
    |}]
;;

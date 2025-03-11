open Roflanml_lib
open Roflanml_lib.Check

let arbitrary_ast =
  QCheck.make
    (QCheck.Gen.sized (fun _ -> Generator.gen_program))
    ~print:(fun prog -> Unparse.unparse_program prog)
    ~shrink:Shrinker.shrink_program
;;

let test =
  QCheck.(
    Test.make ~count:10 arbitrary_ast (fun ast ->
      let src = Unparse.unparse_program ast in
      match Parser.parse src with
      | Result.Ok ast' when Stdlib.( = ) ast' ast -> true
      | Result.Ok ast' ->
        Stdlib.Format.printf
          "\n\n!!! AST DIFFERS !!!\nSource: %S\nParsed: %s\nOriginal: %s\n"
          src
          (Ast.show_program ast')
          (Ast.show_program ast);
        false
      | Result.Error err ->
        Stdlib.Format.printf "\n\n[!]: Parser error: %s\nOn source:\n%S\n" err src;
        false))
;;

let () = QCheck_base_runner.run_tests_main [ test ]

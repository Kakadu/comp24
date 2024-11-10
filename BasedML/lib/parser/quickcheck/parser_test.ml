(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Core

let parser_qtests =
  [ QCheck.(
      Test.make ~count:100 arbitrary_ast (fun before ->
        let src = Restore_src.RestoreSrc.restore_declarations before in
        let res = Parser.parse_program src in
        match res with
        | Result.Ok after when after = before -> true
        | Result.Ok decls ->
          Format.printf "\n\n[!]: Different AST!\n\n";
          Format.printf
            "###Paser ast ### \n%s\n########\n\n"
            (Ast.show_declarations decls);
          Format.printf
            "###Orig ast ### \n%s\n########\n\n"
            (Ast.show_declarations before);
          false
        | Result.Error s ->
          Format.printf "\n\n[!]: Parser error: %s\n" s;
          Format.printf "On test:\n %s\n" src;
          false))
  ]
;;

let%expect_test _ =
  QCheck_runner.set_seed 42;
  let _ = QCheck_runner.run_tests parser_qtests in
  ();
  [%expect {| |}]
;;

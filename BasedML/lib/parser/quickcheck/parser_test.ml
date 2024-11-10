(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Core

let parser_qtests =
  [ QCheck.(
      Test.make ~count:100 arbitrary_ast (fun l ->
        Result.ok l = Parser.parse_program (Restore_src.RestoreSrc.restore_declarations l)))
  ]
;;

let%expect_test _ =
  QCheck_runner.set_seed 42;
  let _ = QCheck_runner.run_tests parser_qtests in
  ();
  [%expect {| |}]
;;

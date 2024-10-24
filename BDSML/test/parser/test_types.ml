(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Test_utils

let test_types str =
  pp_result Parser.Ast.pp_typexpr
  @@ parse_with_parser Parser.Typexpr_parser.parse_typexpr str
;;

let%expect_test "simple test" =
  test_types ": int";
  [%expect {| (Type_single "int") |}]
;;

let%expect_test "simple parents test" =
  test_types ": ( ( int ) )";
  [%expect {| (Type_single "int") |}]
;;

let%expect_test "simple tuple test" =
  test_types ":int*int";
  [%expect {|
    (Type_tuple [(Type_single "int"); (Type_single "int")])
    |}]
;;

let%expect_test "parents tuple test" =
  test_types ":( ( ( int ) * ( int ) ) )";
  [%expect {|
    (Type_tuple [(Type_single "int"); (Type_single "int")])
    |}]
;;

let%expect_test "simple fun test" =
  test_types ":int->int";
  [%expect {|
    (Type_fun [(Type_single "int"); (Type_single "int")])
    |}]
;;

let%expect_test "parents fun test" =
  test_types ":( ( ( int ) -> ( int ) ) )";
  [%expect {|
    (Type_fun [(Type_single "int"); (Type_single "int")])
    |}]
;;

let%expect_test "simple params test" =
  test_types ":a t list";
  [%expect {|
    (Type_params ((Type_params ((Type_single "a"), "t")), "list"))
    |}]
;;

let%expect_test "parents params test" =
  test_types ":( ( ( a ) t ) list )";
  [%expect {|
    (Type_params ((Type_params ((Type_single "a"), "t")), "list"))
    |}]
;;

let%expect_test "params combine test" =
  test_types ":int t list * int -> int -> int";
  [%expect
    {|
    (Type_fun
       [(Type_tuple
           [(Type_params ((Type_params ((Type_single "int"), "t")), "list"));
             (Type_single "int")]);
         (Type_single "int"); (Type_single "int")])
    |}]
;;

let%expect_test "params combine parents test" =
  test_types ":int t list * int -> ((int) -> int) * string * char";
  [%expect
    {|
    (Type_fun
       [(Type_tuple
           [(Type_params ((Type_params ((Type_single "int"), "t")), "list"));
             (Type_single "int")]);
         (Type_tuple
            [(Type_fun [(Type_single "int"); (Type_single "int")]);
              (Type_single "string"); (Type_single "char")])
         ])
    |}]
;;

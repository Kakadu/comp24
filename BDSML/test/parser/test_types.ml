(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

let test_types =
  Test_utils.pp_parse_result Parser.Types_parser.parse_ident Parser.Ast.pp_ident
;;

let%expect_test "simple test" =
  test_types "a: int";
  [%expect {| { name = "a"; id_type = (Some (Type_single "int")) } |}]
;;

let%expect_test "simple parents test" =
  test_types "a: ( ( int ) )";
  [%expect {| { name = "a"; id_type = (Some (Type_single "int")) } |}]
;;

let%expect_test "without test" =
  test_types "a";
  [%expect {| { name = "a"; id_type = None } |}]
;;

let%expect_test "simple tuple test" =
  test_types "a:int*int";
  [%expect
    {|
    { name = "a";
      id_type = (Some (Type_tuple [(Type_single "int"); (Type_single "int")])) }
    |}]
;;

let%expect_test "parents tuple test" =
  test_types "a:( ( ( int ) * ( int ) ) )";
  [%expect
    {|
    { name = "a";
      id_type = (Some (Type_tuple [(Type_single "int"); (Type_single "int")])) }
    |}]
;;

let%expect_test "simple fun test" =
  test_types "a:int->int";
  [%expect
    {|
    { name = "a";
      id_type = (Some (Type_fun [(Type_single "int"); (Type_single "int")])) }
    |}]
;;

let%expect_test "parents fun test" =
  test_types "a:( ( ( int ) -> ( int ) ) )";
  [%expect
    {|
    { name = "a";
      id_type = (Some (Type_fun [(Type_single "int"); (Type_single "int")])) }
    |}]
;;

let%expect_test "simple params test" =
  test_types "a:a t list";
  [%expect
    {|
    { name = "a";
      id_type =
      (Some (Type_params ((Type_params ((Type_single "a"), "t")), "list"))) }
    |}]
;;

let%expect_test "parents params test" =
  test_types "a:( ( ( a ) t ) list )";
  [%expect
    {|
    { name = "a";
      id_type =
      (Some (Type_params ((Type_params ((Type_single "a"), "t")), "list"))) }
    |}]
;;

let%expect_test "params combine test" =
  test_types "a:int t list * int -> int -> int";
  [%expect
    {|
    { name = "a";
      id_type =
      (Some (Type_fun
               [(Type_tuple
                   [(Type_params ((Type_params ((Type_single "int"), "t")),
                       "list"));
                     (Type_single "int")]);
                 (Type_single "int"); (Type_single "int")]))
      }
    |}]
;;

let%expect_test "params combine parents test" =
  test_types "a:int t list * int -> ((int) -> int) * string * char";
  [%expect {|
    { name = "a";
      id_type =
      (Some (Type_fun
               [(Type_tuple
                   [(Type_params ((Type_params ((Type_single "int"), "t")),
                       "list"));
                     (Type_single "int")]);
                 (Type_tuple
                    [(Type_fun [(Type_single "int"); (Type_single "int")]);
                      (Type_single "string"); (Type_single "char")])
                 ]))
      }
    |}]
;;

(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Test_parser.Test_utils
open Quickcheck

let test_pattern_unparser str =
  pp_result Pattern_unparser.unparse_pattern
  @@ parse_with_parser Parser.Pattern_parser.parse_pattern str
;;

let%expect_test "test any" =
  test_pattern_unparser "_";
  [%expect {| _ |}]
;;

let%expect_test "test const int" =
  test_pattern_unparser "   5";
  [%expect {| 5 |}]
;;

let%expect_test "test const string" =
  test_pattern_unparser " \"abraacadabra\"";
  [%expect {| "abraacadabra" |}]
;;

let%expect_test "test const char" =
  test_pattern_unparser " (((\'Z\')))";
  [%expect {| 'Z' |}]
;;

let%expect_test "test const char" =
  test_pattern_unparser " \'Z\' | \'V\'";
  [%expect {| ('Z' | 'V') |}]
;;

let%expect_test "test tuple" =
  test_pattern_unparser "(((1), 2, (((3))), ((4))))";
  [%expect {| (1, 2, 3, 4) |}]
;;

let%expect_test "test list 1" =
  test_pattern_unparser "[1; 2]";
  [%expect {| (1 :: (2 :: [])) |}]
;;

let%expect_test "test list 2" =
  test_pattern_unparser "((1) :: (2) :: [])";
  [%expect {| (1 :: (2 :: [])) |}]
;;

let%expect_test "test list 3" =
  test_pattern_unparser "[ [1] ]";
  [%expect {| ((1 :: []) :: []) |}]
;;

let%expect_test "test list 4" =
  test_pattern_unparser "[[[1]]]";
  [%expect {| (((1 :: []) :: []) :: []) |}]
;;

let%expect_test "test list 5" =
  test_pattern_unparser "[[[1]; [2]]; [[3]; [4]]]";
  [%expect
    {| (((1 :: []) :: ((2 :: []) :: [])) :: (((3 :: []) :: ((4 :: []) :: [])) :: [])) |}]
;;

let%expect_test "cons test" =
  test_pattern_unparser "1 :: 2 :: 3";
  [%expect {| (1 :: (2 :: 3)) |}]
;;

let%expect_test "pat type" =
  test_pattern_unparser "([a]: int list)";
  [%expect {| ((a :: []) : int list) |}]
;;

let%expect_test "or + list" =
  test_pattern_unparser "[a | b; c]";
  [%expect {| ((a | b) :: (c :: [])) |}]
;;

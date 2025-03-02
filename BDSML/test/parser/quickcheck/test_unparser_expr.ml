(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Test_parser.Test_utils
open Quickcheck

let test_expr str =
  pp_result Expr_unparser.unparse_expr
  @@ parse_with_parser Parser.Expr_parser.parse_expr str
;;

let%expect_test "test just sum" =
  test_expr "4 + 5";
  [%expect {| ( + ) 4 5 |}]
;;

let%expect_test "test sum priority" =
  test_expr "4 + 5 + 6";
  [%expect {| ( + ) (( + ) 4 5) 6 |}]
;;

let%expect_test "test sum subs priority" =
  test_expr "4 - 5 + 6 - 7";
  [%expect {| ( - ) (( + ) (( - ) 4 5) 6) 7 |}]
;;

let%expect_test "test sum mult priority" =
  test_expr "4 + 6 * 7";
  [%expect {| ( + ) 4 (( * ) 6 7) |}]
;;

let%expect_test "test my operator" =
  test_expr "4 +== 5";
  [%expect {| ( +== ) 4 5 |}]
;;

let%expect_test "test right associativity" =
  test_expr "2 ** 2 ** 3";
  [%expect {| ( ** ) 2 (( ** ) 2 3) |}]
;;

let%expect_test "test unary prefix op" =
  test_expr "! 4";
  [%expect {| ! 4 |}]
;;

let%expect_test "test several unary prefix op" =
  test_expr "! ! 4";
  [%expect {| ! (! 4) |}]
;;

let%expect_test "test parents priority" =
  test_expr "4 * (5 + 6)";
  [%expect {| ( * ) 4 (( + ) 5 6) |}]
;;

let%expect_test "test application" =
  test_expr "camel by camel";
  [%expect {| camel by camel |}]
;;

let%expect_test "test prefix two args application" =
  test_expr "! 1 2";
  [%expect {| ! 1 2 |}]
;;

let%expect_test "test prefix two args application" =
  test_expr "! ! 1 2 4";
  [%expect {| ! (! 1) 2 4 |}]
;;

let%expect_test "test binary infix with prefix" =
  test_expr "2 + - 1 + 3";
  [%expect {| ( + ) (( + ) 2 (( ~- ) 1)) 3 |}]
;;

let%expect_test "test tuple" =
  test_expr "1, 2";
  [%expect {| (1, 2) |}]
;;

let%expect_test "test tuple parents" =
  test_expr "(1, 2)";
  [%expect {| (1, 2) |}]
;;

let%expect_test "test tuple in tuple" =
  test_expr "((1, 3), 2+4)";
  [%expect {| ((1, 3), ( + ) 2 4) |}]
;;

let%expect_test "test empty constructor" =
  test_expr "BDSML";
  [%expect {| BDSML |}]
;;

let%expect_test "test trivial constructor" =
  test_expr "BDSML 1";
  [%expect {| BDSML 1 |}]
;;

let%expect_test "test tuple constructor" =
  test_expr "BDSML (1, 2)";
  [%expect {| BDSML (1, 2) |}]
;;

let%expect_test "test tuple constructor" =
  test_expr "BDSML 1, 2";
  [%expect {| (BDSML 1, 2) |}]
;;

let%expect_test "test let in" =
  test_expr "let 4 = 4 in b";
  [%expect {| let 4 = 4 in b |}]
;;

let%expect_test "test let several" =
  test_expr "let rec a = 4 and b = 6 in a+b";
  [%expect {| let rec a = 4 and b = 6 in ( + ) a b |}]
;;

let%expect_test "test let in let" =
  test_expr "let a = let b = 4 in b in a";
  [%expect {| let a = let b = 4 in b in a |}]
;;

let%expect_test "test num + let" =
  test_expr "4 + let a = 5 in a";
  [%expect {| ( + ) 4 (let a = 5 in a) |}]
;;

let%expect_test "test fun" =
  test_expr "fun a b -> a+b";
  [%expect {| fun a b -> ( + ) a b |}]
;;

let%expect_test "test fun tuple" =
  test_expr "4, fun a -> a";
  [%expect {| (4, fun a -> a) |}]
;;

let%expect_test "test function case 1" =
  test_expr "function | a -> a | _ -> b";
  [%expect {| function | a -> a | _ -> b |}]
;;

let%expect_test "test function case 2" =
  test_expr "function a -> a | 1 | 2 -> b";
  [%expect {| function | a -> a | (1 | 2) -> b |}]
;;

let%expect_test "test if" =
  test_expr "if a then b else c";
  [%expect {| if a then b else c |}]
;;

let%expect_test "test if without else" =
  test_expr "if a then b";
  [%expect {| if a then b |}]
;;

let%expect_test "test if in expr" =
  test_expr "4 + if a then b else c";
  [%expect {| ( + ) 4 (if a then b else c) |}]
;;

let%expect_test "test for if by Andrey Sukhorev 1" =
  test_expr "if a then b; c";
  [%expect
    {|
    if a then b;
    c
    |}]
;;

let%expect_test "test for if by Andrey Sukhorev 2" =
  test_expr "if a then b; c else d";
  [%expect {| Error: end_of_input |}]
;;

let%expect_test "test for if by Andrey Sukhorev 3" =
  test_expr "if a then b else c; d";
  [%expect
    {|
    if a then b else c;
    d
    |}]
;;

let%expect_test "test match" =
  test_expr "match 4 with|4 -> 1";
  [%expect {| match 4 with | 4 -> 1 |}]
;;

let%expect_test "test cons list" =
  test_expr "1 :: 2 :: []";
  [%expect {| (::) (1, (::) (2, [])) |}]
;;

let%expect_test "test list" =
  test_expr "[ 1; 2 ]";
  [%expect {| (::) (1, (::) (2, [])) |}]
;;

let%expect_test "test list ops inside" =
  test_expr "[ 4 > 3; 3 > 2 ]";
  [%expect {| (::) (( > ) 4 3, (::) (( > ) 3 2, [])) |}]
;;

let%expect_test "test typexpr" =
  test_expr "(4: int)";
  [%expect {| (4 : int) |}]
;;

let%expect_test "test typexpr 2" =
  test_expr "(4 + 5: int)";
  [%expect {| (( + ) 4 5 : int) |}]
;;

let%expect_test "test redefine operator" =
  test_expr "let (+) a b = a - b in let (!) m = m in 5 + !4";
  [%expect {| let ( + ) a b = ( - ) a b in let ( ! ) m = m in ( + ) 5 (! 4) |}]
;;

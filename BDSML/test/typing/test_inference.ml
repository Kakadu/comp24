open Test_utils

let%expect_test "test int inference" =
  test "4";
  [%expect {| int |}]
;;

let%expect_test "test char inference" =
  test "'c'";
  [%expect {| char |}]
;;

let%expect_test "test string inference" =
  test "\"help me plz\"";
  [%expect {| string |}]
;;

let%expect_test "test if inference" =
  test "if true then 4 else 5";
  [%expect {| int |}]
;;

let%expect_test "test wrong if inference" =
  test "if false then 4 else 'c'";
  [%expect {| ErrorType infering error: failed unification of types int and char |}]
;;

let%expect_test "test if without else inference" =
  test "if true then 4";
  [%expect {| int |}]
;;

let%expect_test "test fun inference" =
  test "fun a -> a";
  [%expect {| 'a -> 'a |}]
;;

let%expect_test "test fun with several arguments inference" =
  test "fun a b c -> b";
  [%expect {| 'a -> 'b -> 'c -> 'b |}]
;;

let%expect_test "test let fun" =
  test "let a b = b in a";
  [%expect {| 'b -> 'b |}]
;;

let%expect_test "test let fun apply" =
  test "let a b = b in a 4";
  [%expect {| int |}]
;;

let%expect_test "test simple let" =
  test "let a = 4 in a";
  [%expect {| int |}]
;;

let%expect_test "test several lets" =
  test "let f a = a and a = true and b = 4 in f f b";
  [%expect {| int |}]
;;

let%expect_test "test tuple" =
  test "(4, 5, true, 'a')";
  [%expect {| (int * int * bool * char) |}]
;;

let%expect_test "test difficult tuple" =
  test "(let b = 4 in b, (3, 5), fun a -> a, 'a')";
  [%expect {| (int * (int * int) * 'a -> ('a * char)) |}]
;;

let%expect_test "test let rec" =
  test "let rec a = 4 in a";
  [%expect {| int |}]
;;

let%expect_test "test let rec fun" =
  test "let rec f a = if a then a else f a in f";
  [%expect {| bool -> bool |}]
;;

let%expect_test "test let rec and" =
  test {|let rec f a b = if b then g a b else m
  and g a b = f a b and m = 4 in f|};
  [%expect {| 'h -> bool -> int |}]
;;

let%expect_test "test invalid let rec with pat binding" =
  test {|let rec a, b = 3, 4 in a|};
  [%expect
    {| ErrorType infering error: Only variables are allowed as left-hand side of `let rec' |}]
;;

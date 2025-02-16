open Test_utils

let%expect_test "test int inference" =
  test "4";
  [%expect {| (TBase TInt) |}]
;;

let%expect_test "test char inference" =
  test "'c'";
  [%expect {| (TBase TChar) |}]
;;

let%expect_test "test string inference" =
  test "\"help me plz\"";
  [%expect {| (TBase TString) |}]
;;

let%expect_test "test if inference" =
  test "if true then 4 else 5";
  [%expect {| (TBase TInt) |}]
;;

let%expect_test "test wrong if inference" =
  test "if false then 4 else 'c'";
  [%expect
    {| ErrorType infering error: failed unification of types (TBase TInt) and (TBase TChar) |}]
;;

let%expect_test "test if without else inference" =
  test "if true then 4";
  [%expect {| (TBase TInt) |}]
;;

let%expect_test "test fun inference" =
  test "fun a -> a";
  [%expect {| (TArrow ((TVar 1), (TVar 1))) |}]
;;

let%expect_test "test fun with several arguments inference" =
  test "fun a b c -> b";
  [%expect {| (TArrow ((TVar 1), (TArrow ((TVar 2), (TArrow ((TVar 3), (TVar 2))))))) |}]
;;

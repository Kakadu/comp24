open Test_utils

let%expect_test "test int inference" =
  test "4";
  [%expect {| 0 (TBase TInt) |}]
;;

let%expect_test "test char inference" =
  test "'c'";
  [%expect {| 0 (TBase TChar) |}]
;;

let%expect_test "test string inference" =
  test "\"help me plz\"";
  [%expect {| 0 (TBase TString) |}]
;;

let%expect_test "test if inference" =
  test "if true then 4 else 5";
  [%expect {| 0 (TBase TInt) |}]
;;

let%expect_test "test wrong if inference" =
  test "if false then 4 else 'c'";
  [%expect {| ErrorType infering error: failed unification of types (TBase TInt) and (TBase TChar) |}]
;;

let%expect_test "test if without else inference" =
  test "if true then 4";
  [%expect {| 0 (TBase TInt) |}]
;;

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

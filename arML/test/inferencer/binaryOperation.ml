(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

let%expect_test _ =
  inference {| 1 + 1 |};
  [%expect
    {|
    - : int |}]
;;

let%expect_test _ =
  inference {| 1 * 1 |};
  [%expect
    {|
    - : int |}]
;;

let%expect_test _ =
  inference {| 1 / 1 |};
  [%expect
    {|
    - : int |}]
;;

let%expect_test _ =
  inference {| 1 % 1 |};
  [%expect
    {|
    - : int |}]
;;

let%expect_test _ =
  inference {| 1 + 1 + 1 + 1 + 1 |};
  [%expect
    {|
    - : int |}]
;;

let%expect_test _ =
  inference {| (1 / 2 + 3 * 8 / 9 % 3) * 3 - 1 |};
  [%expect
    {|
    - : int |}]
;;

let%expect_test _ =
  inference {| true || false |};
  [%expect
    {|
    - : bool |}]
;;

let%expect_test _ =
  inference {| true && false |};
  [%expect
    {|
    - : bool |}]
;;

let%expect_test _ =
  inference {| (true && false) || (false && true) |};
  [%expect
    {|
    - : bool |}]
;;

let%expect_test _ =
  inference {| (true || false) && (false || true) |};
  [%expect
    {|
    - : bool |}]
;;

let%expect_test _ =
  inference {| 1 <= 2 |};
  [%expect
    {|
    - : bool |}]
;;

let%expect_test _ =
  inference {| 1 >= 2 |};
  [%expect
    {|
    - : bool |}]
;;

let%expect_test _ =
  inference {| 1 = 1 && (1 != 2 || 1 <> 2) |};
  [%expect
    {|
    - : bool |}]
;;

let%expect_test _ =
  inference {| (1 >= 1 || 1 <= 1) = true |};
  [%expect
    {|
    - : bool |}]
;;

let%expect_test _ =
  inference {| "aaa" = "aaa" |};
  [%expect
    {|
    - : bool |}]
;;

let%expect_test _ =
  inference {| "aaa" < "bbb" |};
  [%expect
    {|
    - : bool |}]
;;

let%expect_test _ =
  inference {| () = () |};
  [%expect
    {|
    - : bool |}]
;;

let%expect_test _ =
  inference {| true <> false && true != false |};
  [%expect
    {|
    - : bool |}]
;;

let%expect_test _ =
  inference {| 1 + 3 >= 4 && 1 - 3 <= 9 |};
  [%expect
    {|
    - : bool |}]
;;
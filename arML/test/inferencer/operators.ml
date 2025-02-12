(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

(* Binary operartions *)

let%expect_test _ =
  inference_expression {| 1 + 1 |};
  [%expect {|
    - : int |}]
;;

let%expect_test _ =
  inference_expression {| 1 * 1 |};
  [%expect {|
    - : int |}]
;;

let%expect_test _ =
  inference_expression {| 1 / 1 |};
  [%expect {|
    - : int |}]
;;

let%expect_test _ =
  inference_expression {| 1 % 1 |};
  [%expect {|
    - : int |}]
;;

let%expect_test _ =
  inference_expression {| 1 + 1 + 1 + 1 + 1 |};
  [%expect {|
    - : int |}]
;;

let%expect_test _ =
  inference_expression {| (1 / 2 + 3 * 8 / 9 % 3) * 3 - 1 |};
  [%expect {|
    - : int |}]
;;

let%expect_test _ =
  inference_expression {| true || false |};
  [%expect {|
    - : bool |}]
;;

let%expect_test _ =
  inference_expression {| true && false |};
  [%expect {|
    - : bool |}]
;;

let%expect_test _ =
  inference_expression {| (true && false) || (false && true) |};
  [%expect {|
    - : bool |}]
;;

let%expect_test _ =
  inference_expression {| (true || false) && (false || true) |};
  [%expect {|
    - : bool |}]
;;

let%expect_test _ =
  inference_expression {| 1 <= 2 |};
  [%expect {|
    - : bool |}]
;;

let%expect_test _ =
  inference_expression {| 1 >= 2 |};
  [%expect {|
    - : bool |}]
;;

let%expect_test _ =
  inference_expression {| 1 = 1 && (1 != 2 || 1 <> 2) |};
  [%expect {|
    - : bool |}]
;;

let%expect_test _ =
  inference_expression {| (1 >= 1 || 1 <= 1) = true |};
  [%expect {|
    - : bool |}]
;;

let%expect_test _ =
  inference_expression {| "aaa" = "aaa" |};
  [%expect {|
    - : bool |}]
;;

let%expect_test _ =
  inference_expression {| "aaa" < "bbb" |};
  [%expect {|
    - : bool |}]
;;

let%expect_test _ =
  inference_expression {| () = () |};
  [%expect {|
    - : bool |}]
;;

let%expect_test _ =
  inference_expression {| true <> false && true != false |};
  [%expect {|
    - : bool |}]
;;

let%expect_test _ =
  inference_expression {| 1 + 3 >= 4 && 1 - 3 <= 9 |};
  [%expect {|
    - : bool |}]
;;

(* ---------------- *)

(* Unary operartions *)

let%expect_test _ =
  inference_expression {| -1 |};
  [%expect
    {|
    - : int |}]
;;

let%expect_test _ =
  inference_expression {| +1 |};
  [%expect
    {|
    - : int |}]
;;

let%expect_test _ =
  inference_expression {| not true |};
  [%expect
    {|
    - : bool |}]
;;

let%expect_test _ =
  inference_expression {| fun f -> f (-1) |};
  [%expect
    {|
    - : (int -> 'a) -> 'a |}]
;;

let%expect_test _ =
  inference_expression {| not (1 + (-2) * (+3) >= 3) |};
  [%expect
    {|
    - : bool |}]
;;

(* ---------------- *)
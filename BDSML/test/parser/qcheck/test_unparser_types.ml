(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Test_parser.Test_utils
open Qcheck

let test_types_unparser str =
  pp_result Typexpr_unparser.unparse_typexpr
  @@ parse_with_parser Parser.Typexpr_parser.parse_typexpr str
;;

let%expect_test "simple test" =
  test_types_unparser ": int";
  [%expect {| int |}]
;;

let%expect_test "simple tuple test" =
  test_types_unparser ":int*int";
  [%expect {| int * int |}]
;;

let%expect_test "simple fun test" =
  test_types_unparser ":int->int";
  [%expect {| (int -> int) |}]
;;

let%expect_test "simple params test" =
  test_types_unparser ":a t list";
  [%expect {| a t list |}]
;;

let%expect_test "params combine test" =
  test_types_unparser ":int t list * int -> int -> int";
  [%expect {| (int t list * int -> int -> int) |}]
;;

let%expect_test "params combine parents test" =
  test_types_unparser ":int t list * int -> ((int) -> int) * string * char";
  [%expect {| (int t list * int -> (int -> int) * string * char) |}]
;;

(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Test_parser.Test_utils
open Qcheck

let test_parser str =
  pp_result Main_unparser.unparse_structure @@ Parser.Main_parser.parse str
;;

let%expect_test "some bruh test" =
  test_parser
    {|
let (+) a b = a - b;;
let m = 4 and mm = 6;;
m + let b = 6 in m + b;;
|};
  [%expect
    {|
    let ( + ) a b = ( - ) a b;;
    let m = 4 and mm = 6;;
    ( + ) m (let b = 6 in ( + ) m b);;
    |}]
;;

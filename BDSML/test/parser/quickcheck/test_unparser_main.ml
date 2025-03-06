(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Quickcheck

let test_parser str =
  print_string
    (match Parser.Main_parser.parse str with
     | Result.Ok s -> Main_unparser.unparse_structure s
     | Result.Error e -> e)
;;

let%expect_test "some bruh test" =
  test_parser {|
let (+) a b = a - b;;
let m = 4 and mm = 6;;
m + let b = 6 in m + b;;
|};
  [%expect
    {|
    let ( + ) a b = ((( - ) a) b);;
    let m = 4
     and mm = 6;;
    ((( + ) m) ((let b = 6 in
     ((( + ) m) b))));;
    |}]
;;

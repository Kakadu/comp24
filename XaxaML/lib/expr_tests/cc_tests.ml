(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open XaxaML

let cc_expr_and_print_ast str =
  let parsed = Result.get_ok (Parser.run_parser_expr str) in
  let _ = Result.get_ok (Inferencer.run_infer_expr parsed) in
  let new_expr = Closure_conversion.run_closure_conversion_expr parsed in
  Format.printf "%a" Pprint_as_src.pp_expr_as_src new_expr
;;

let%expect_test "do nothing 1" =
  cc_expr_and_print_ast {|1 + 2 * 3|};
  [%expect {|((+ 1) ((* 2) 3)) |}]
;;

let%expect_test "do nothing 2" =
  cc_expr_and_print_ast {|fun a b -> a + b|};
  [%expect {|(fun a b -> ((+ a) b)) |}]
;;

let%expect_test "do nothing 2" =
  cc_expr_and_print_ast {|let x = 1 in let f x = x in f 1|};
  [%expect {|
    let x = 1 in
    let f = (fun x -> x) in
    (f 1) |}]
;;

let%expect_test "anonymous fun, add c d" =
  cc_expr_and_print_ast {|let (c, d) = (1, 2) in fun a -> a + c + d|};
  [%expect {|
    let (c, d) = (1, 2) in
    (((fun c d a -> ((+ ((+ a) c)) d)) c) d) |}]
;;

let%expect_test "add one arg with applying" =
  cc_expr_and_print_ast {|let x = 1 in let f y = x + y in f 1|};
  [%expect {|
    let x = 1 in
    let f = (fun x y -> ((+ x) y)) in
    ((f x) 1) |}]
;;

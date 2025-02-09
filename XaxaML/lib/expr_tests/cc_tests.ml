(** Copyright 2025, aartdem, toadharvard *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open XaxaML

let cc_expr_and_print_ast str =
  let parsed = Result.get_ok (Parser.run_parser_expr str) in
  let _ = Result.get_ok (Inferencer.run_infer_expr parsed) in
  let new_expr = Closure_conversion.run_closure_conversion_expr parsed in
  Format.printf "%a" Ast.pp_expr new_expr
;;

let%expect_test "do nothing 1" =
  cc_expr_and_print_ast {|1 + 2 * 3|};
  [%expect
    {|
    (E_app ((E_app ((E_ident "+"), (E_const (C_int 1)))),
       (E_app ((E_app ((E_ident "*"), (E_const (C_int 2)))), (E_const (C_int 3))
          ))
       )) |}]
;;

let%expect_test "do nothing 2" =
  cc_expr_and_print_ast {|fun a b -> a + b|};
  [%expect
    {|
    (E_fun ((P_val "a"), [(P_val "b")],
       (E_app ((E_app ((E_ident "+"), (E_ident "a"))), (E_ident "b"))))) |}]
;;

let%expect_test "anonymous fun, add c d" =
  cc_expr_and_print_ast {|let (c, d) = (1, 2) in fun a -> a + c + d|};
  [%expect
    {|
    (E_let (
       (Non_rec
          ((P_tuple ((P_val "c"), [(P_val "d")])), None,
           (E_tuple ((E_const (C_int 1)), [(E_const (C_int 2))])))),
       (E_fun ((P_val "c"), [(P_val "d"); (P_val "a")],
          (E_app (
             (E_app ((E_ident "+"),
                (E_app ((E_app ((E_ident "+"), (E_ident "a"))), (E_ident "c"))))),
             (E_ident "d")))
          ))
       )) |}]
;;

let%expect_test "add one arg with applying" =
  cc_expr_and_print_ast {|let x = 1 in let f y = x + y in f 1 |};
  [%expect
    {|
    (E_let ((Non_rec ((P_val "x"), None, (E_const (C_int 1)))),
       (E_let (
          (Non_rec
             ((P_val "f"), None,
              (E_fun ((P_val "x"), [(P_val "y")],
                 (E_app ((E_app ((E_ident "+"), (E_ident "x"))), (E_ident "y")))
                 )))),
          (E_app ((E_app ((E_ident "f"), (E_ident "x"))), (E_const (C_int 1))))))
       )) |}]
;;

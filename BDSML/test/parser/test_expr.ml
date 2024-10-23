(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

let test_expr =
  Test_utils.pp_parse_result Parser.Expr_parser.parse_expr Parser.Ast.pp_expression
;;

let%expect_test "test just sum" =
  test_expr "4 + 5";
  [%expect
    {|
    (Exp_apply ((Exp_ident "+"),
       (Exp_tuple [(Exp_constant (Const_int 4)); (Exp_constant (Const_int 5))])))
    |}]
;;

let%expect_test "test sum priority" =
  test_expr "4 + 5 + 6";
  [%expect
    {|
    (Exp_apply ((Exp_ident "+"),
       (Exp_tuple
          [(Exp_apply ((Exp_ident "+"),
              (Exp_tuple
                 [(Exp_constant (Const_int 4)); (Exp_constant (Const_int 5))])
              ));
            (Exp_constant (Const_int 6))])
       ))
    |}]
;;

let%expect_test "test sum subs priority" =
  test_expr "4 - 5 + 6 - 7";
  [%expect
    {|
    (Exp_apply ((Exp_ident "-"),
       (Exp_tuple
          [(Exp_apply ((Exp_ident "+"),
              (Exp_tuple
                 [(Exp_apply ((Exp_ident "-"),
                     (Exp_tuple
                        [(Exp_constant (Const_int 4));
                          (Exp_constant (Const_int 5))])
                     ));
                   (Exp_constant (Const_int 6))])
              ));
            (Exp_constant (Const_int 7))])
       ))
    |}]
;;

let%expect_test "test sum mult priority" =
  test_expr "4 + 6 * 7";
  [%expect
    {|
    (Exp_apply ((Exp_ident "+"),
       (Exp_tuple
          [(Exp_constant (Const_int 4));
            (Exp_apply ((Exp_ident "*"),
               (Exp_tuple
                  [(Exp_constant (Const_int 6)); (Exp_constant (Const_int 7))])
               ))
            ])
       ))
    |}]
;;

let%expect_test "test my operator" =
  test_expr "4 +== 5";
  [%expect
    {|
    (Exp_apply ((Exp_ident "+=="),
       (Exp_tuple [(Exp_constant (Const_int 4)); (Exp_constant (Const_int 5))])))
    |}]
;;

let%expect_test "test right associativity" =
  test_expr "4 ** 5 ** 6";
  [%expect
    {|
    (Exp_apply ((Exp_ident "**"),
       (Exp_tuple
          [(Exp_constant (Const_int 4));
            (Exp_apply ((Exp_ident "**"),
               (Exp_tuple
                  [(Exp_constant (Const_int 5)); (Exp_constant (Const_int 6))])
               ))
            ])
       ))
    |}]
;;

let%expect_test "test unary prefix op" =
  test_expr "! 4";
  [%expect
    {| (Exp_apply ((Exp_ident "!"), (Exp_tuple [(Exp_constant (Const_int 4))]))) |}]
;;

let%expect_test "test several unary prefix op" =
  test_expr "! ! 4";
  [%expect
    {|
    (Exp_apply ((Exp_ident "!"),
       (Exp_tuple [(Exp_apply ((Exp_ident "!"), (Exp_constant (Const_int 4))))])
       ))
    |}]
;;

let%expect_test "test parents priority" =
  test_expr "4 + (5 + 6)";
  [%expect
    {|
    (Exp_apply ((Exp_ident "+"),
       (Exp_tuple
          [(Exp_constant (Const_int 4));
            (Exp_apply ((Exp_ident "+"),
               (Exp_tuple
                  [(Exp_constant (Const_int 5)); (Exp_constant (Const_int 6))])
               ))
            ])
       ))
    |}]
;;

let%expect_test "test parents priority different operators" =
  test_expr "4 * (5 + 6)";
  [%expect
    {|
    (Exp_apply ((Exp_ident "*"),
       (Exp_tuple
          [(Exp_constant (Const_int 4));
            (Exp_apply ((Exp_ident "+"),
               (Exp_tuple
                  [(Exp_constant (Const_int 5)); (Exp_constant (Const_int 6))])
               ))
            ])
       ))
    |}]
;;

let%expect_test "test application" =
  test_expr "camel by camel";
  [%expect
    {|
    (Exp_apply ((Exp_ident "camel"),
       (Exp_tuple [(Exp_ident "by"); (Exp_ident "camel")])))
    |}]
;;

let%expect_test "test prefix two args application" =
  test_expr "! 1 2";
  [%expect
    {|
    (Exp_apply ((Exp_ident "!"),
       (Exp_tuple [(Exp_constant (Const_int 1)); (Exp_constant (Const_int 2))])))
    |}]
;;

let%expect_test "test prefix two args application" =
  test_expr "! ! 1 2 4";
  [%expect
    {|
    (Exp_apply ((Exp_ident "!"),
       (Exp_tuple
          [(Exp_apply ((Exp_ident "!"), (Exp_constant (Const_int 1))));
            (Exp_constant (Const_int 2)); (Exp_constant (Const_int 4))])
       ))
    |}]
;;

let%expect_test "test binary infix with prefix" =
  test_expr "2 + - 1 + 3";
  [%expect
    {|
    (Exp_apply ((Exp_ident "+"),
       (Exp_tuple
          [(Exp_apply ((Exp_ident "+"),
              (Exp_tuple
                 [(Exp_constant (Const_int 2));
                   (Exp_apply ((Exp_ident "-"), (Exp_constant (Const_int 1))))])
              ));
            (Exp_constant (Const_int 3))])
       ))
    |}]
;;

let%expect_test "test binary prefix call" =
  test_expr "(+) 2 3";
  [%expect
    {|
    (Exp_apply ((Exp_ident "+"),
       (Exp_tuple [(Exp_constant (Const_int 2)); (Exp_constant (Const_int 3))])))
    |}]
;;

let%expect_test "test just identifier" =
  test_expr "y_combinator";
  [%expect {| (Exp_ident "y_combinator") |}]
;;

let%expect_test "test by Andrey Sukharev" =
  test_expr "a >>= b ++ c ** d !+ e";
  [%expect {|
    (Exp_apply ((Exp_ident ">>="),
       (Exp_tuple
          [(Exp_ident "a");
            (Exp_apply ((Exp_ident "++"),
               (Exp_tuple
                  [(Exp_ident "b");
                    (Exp_apply ((Exp_ident "**"),
                       (Exp_tuple
                          [(Exp_ident "c");
                            (Exp_apply ((Exp_ident "d"),
                               (Exp_tuple
                                  [(Exp_apply ((Exp_ident "!+"), (Exp_ident "e")
                                      ))
                                    ])
                               ))
                            ])
                       ))
                    ])
               ))
            ])
       ))
    |}]
;;

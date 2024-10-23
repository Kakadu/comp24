(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

let test_pattern =
  Test_utils.pp_parse_result Parser.Expr_parser.parse_expr Parser.Ast.pp_expression
;;

let%expect_test "test just sum" =
  test_pattern "4 + 5";
  [%expect
    {|
    (Exp_apply ((Exp_ident "+"),
       (Exp_tuple [(Exp_constant (Const_int 4)); (Exp_constant (Const_int 5))]))) |}]
;;

let%expect_test "test sum priority" =
  test_pattern "4 + 5 + 6";
  [%expect
    {|
    (Exp_apply ((Exp_ident "+"),
       (Exp_tuple
          [(Exp_apply ((Exp_ident "+"),
              (Exp_tuple
                 [(Exp_constant (Const_int 4)); (Exp_constant (Const_int 5))])
              ));
            (Exp_constant (Const_int 6))])
       )) |}]
;;

let%expect_test "test sum subs priority" =
  test_pattern "4 - 5 + 6 - 7";
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
  test_pattern "4 + 6 * 7";
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
  test_pattern "4 +== 5";
  [%expect
    {|
    (Exp_apply ((Exp_ident "+=="),
       (Exp_tuple [(Exp_constant (Const_int 4)); (Exp_constant (Const_int 5))])))
   |}]
;;

let%expect_test "test right associativity" =
  test_pattern "4 ** 5 ** 6";
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
  test_pattern "! 4";
  [%expect {| (Exp_apply ((Exp_ident "!"), (Exp_constant (Const_int 4)))) |}]
;;

let%expect_test "test several unary prefix op" =
  test_pattern "! ! 4";
  [%expect {|
    (Exp_apply ((Exp_ident "!"),
       (Exp_apply ((Exp_ident "!"), (Exp_constant (Const_int 4))))))
    |}]
;;

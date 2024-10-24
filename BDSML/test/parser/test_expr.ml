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
  [%expect
    {|
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

let%expect_test "test tuple" =
  test_expr "1, 2";
  [%expect {| (Exp_tuple [(Exp_constant (Const_int 1)); (Exp_constant (Const_int 2))]) |}]
;;

let%expect_test "test tuple parents" =
  test_expr "(1, 2)";
  [%expect {| (Exp_tuple [(Exp_constant (Const_int 1)); (Exp_constant (Const_int 2))]) |}]
;;

let%expect_test "test tuple in tuple" =
  test_expr "((1, 3), 2+4)";
  [%expect
    {|
    (Exp_tuple
       [(Exp_tuple [(Exp_constant (Const_int 1)); (Exp_constant (Const_int 3))]);
         (Exp_apply ((Exp_ident "+"),
            (Exp_tuple
               [(Exp_constant (Const_int 2)); (Exp_constant (Const_int 4))])
            ))
         ]) |}]
;;

let%expect_test "test empty constructor" =
  test_expr "BDSML";
  [%expect {| (Exp_construct ("BDSML", None)) |}]
;;

let%expect_test "test trivial constructor" =
  test_expr "BDSML 1";
  [%expect {| (Exp_construct ("BDSML", (Some (Exp_constant (Const_int 1))))) |}]
;;

let%expect_test "test tuple constructor" =
  test_expr "BDSML (1, 2)";
  [%expect
    {|
    (Exp_construct ("BDSML",
       (Some (Exp_tuple
                [(Exp_constant (Const_int 1)); (Exp_constant (Const_int 2))]))
       )) |}]
;;

let%expect_test "test tuple constructor" =
  test_expr "BDSML 1, 2";
  [%expect
    {|
    (Exp_tuple
       [(Exp_construct ("BDSML", (Some (Exp_constant (Const_int 1)))));
         (Exp_constant (Const_int 2))]) |}]
;;

let%expect_test "test let in" =
  test_expr "let 4 = 4 in b";
  [%expect
    {|
    (Exp_let (Nonrecursive,
       [{ pat = (Pat_constant (Const_int 4)); expr = (Exp_constant (Const_int 4))
          }
         ],
       (Exp_ident "b")))
     |}]
;;

let%expect_test "test let several" =
  test_expr "let rec a = 4 and b = 6 in a+b";
  [%expect
    {|
    (Exp_let (Recursive,
       [{ pat = (Pat_var "a"); expr = (Exp_constant (Const_int 4)) };
         { pat = (Pat_var "b"); expr = (Exp_constant (Const_int 6)) }],
       (Exp_apply ((Exp_ident "+"),
          (Exp_tuple [(Exp_ident "a"); (Exp_ident "b")])))
       ))
     |}]
;;

let%expect_test "test let in let" =
  test_expr "let a = let b = 4 in b in a";
  [%expect
    {|
    (Exp_let (Nonrecursive,
       [{ pat = (Pat_var "a");
          expr =
          (Exp_let (Nonrecursive,
             [{ pat = (Pat_var "b"); expr = (Exp_constant (Const_int 4)) }],
             (Exp_ident "b")))
          }
         ],
       (Exp_ident "a")))
     |}]
;;

let%expect_test "test num + let" =
  test_expr "4 + let a = 5 in a";
  [%expect
    {|
    (Exp_apply ((Exp_ident "+"),
       (Exp_tuple
          [(Exp_constant (Const_int 4));
            (Exp_let (Nonrecursive,
               [{ pat = (Pat_var "a"); expr = (Exp_constant (Const_int 5)) }],
               (Exp_ident "a")))
            ])
       ))
     |}]
;;

let%expect_test "test fun" =
  test_expr "fun a b -> a+b";
  [%expect
    {|
    (Exp_fun ([(Pat_var "a"); (Pat_var "b")],
       (Exp_apply ((Exp_ident "+"),
          (Exp_tuple [(Exp_ident "a"); (Exp_ident "b")])))
       ))
     |}]
;;

let%expect_test "test fun tuple" =
  test_expr "4, fun a -> a";
  [%expect
    {|
    (Exp_tuple
       [(Exp_constant (Const_int 4));
         (Exp_fun ([(Pat_var "a")], (Exp_ident "a")))])
     |}]
;;

let%expect_test "test function case 1" =
  test_expr "function | a -> a | _ -> b";
  [%expect
    {|
    (Exp_function
       [{ left = (Pat_var "a"); right = (Exp_ident "a") };
         { left = Pat_any; right = (Exp_ident "b") }])
     |}]
;;

let%expect_test "test function case 2" =
  test_expr "function a -> a | 1 | 2 -> b";
  [%expect
    {|
    (Exp_function
       [{ left = (Pat_var "a"); right = (Exp_ident "a") };
         { left =
           (Pat_or ((Pat_constant (Const_int 1)), (Pat_constant (Const_int 2))));
           right = (Exp_ident "b") }
         ])
     |}]
;;

let%expect_test "test if" =
  test_expr "if a then b else c";
  [%expect
    {|
    (Exp_if ((Exp_ident "a"), (Exp_ident "b"), (Some (Exp_ident "c"))))
     |}]
;;

let%expect_test "test if without else" =
  test_expr "if a then b";
  [%expect {|
    (Exp_if ((Exp_ident "a"), (Exp_ident "b"), None))
     |}]
;;

let%expect_test "test if in expr" =
  test_expr "4 + if a then b else c";
  [%expect
    {|
    (Exp_apply ((Exp_ident "+"),
       (Exp_tuple
          [(Exp_constant (Const_int 4));
            (Exp_if ((Exp_ident "a"), (Exp_ident "b"), (Some (Exp_ident "c"))))])
       ))
     |}]
;;

let%expect_test "test for if by Andrey Sukhorev 1" =
  test_expr "if a then b; c";
  [%expect
    {|
    (Exp_sequence ((Exp_if ((Exp_ident "a"), (Exp_ident "b"), None)),
       (Exp_ident "c")))
     |}]
;;

let%expect_test "test for if by Andrey Sukhorev 2" =
  test_expr "if a then b; c else d";
  [%expect {| Error: end_of_input |}]
;;

let%expect_test "test for if by Andrey Sukhorev 3" =
  test_expr "if a then b else c; d";
  [%expect {|
    (Exp_sequence (
       (Exp_if ((Exp_ident "a"), (Exp_ident "b"), (Some (Exp_ident "c")))),
       (Exp_ident "d")))
     |}]
;;

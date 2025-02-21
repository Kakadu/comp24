(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Test_utils

let test_expr str =
  pp_result Parser.Ast.pp_expression
  @@ parse_with_parser Parser.Expr_parser.parse_expr str
;;

let%expect_test "test just sum" =
  test_expr "4 + 5";
  [%expect
    {|
    (Exp_apply ((Exp_apply ((Exp_ident "( + )"), (Exp_constant (Const_int 4)))),
       (Exp_constant (Const_int 5))))
    |}]
;;

let%expect_test "test bool" =
  test_expr "true, false";
  [%expect
    {|
    (Exp_tuple
       [(Exp_constant (Const_bool true)); (Exp_constant (Const_bool false))])
    |}]
;;

let%expect_test "test sum priority" =
  test_expr "4 + 5 + 6";
  [%expect
    {|
    (Exp_apply (
       (Exp_apply ((Exp_ident "( + )"),
          (Exp_apply (
             (Exp_apply ((Exp_ident "( + )"), (Exp_constant (Const_int 4)))),
             (Exp_constant (Const_int 5))))
          )),
       (Exp_constant (Const_int 6))))
    |}]
;;

let%expect_test "test sum subs priority" =
  test_expr "4 - 5 + 6 - 7";
  [%expect
    {|
    (Exp_apply (
       (Exp_apply ((Exp_ident "( - )"),
          (Exp_apply (
             (Exp_apply ((Exp_ident "( + )"),
                (Exp_apply (
                   (Exp_apply ((Exp_ident "( - )"), (Exp_constant (Const_int 4))
                      )),
                   (Exp_constant (Const_int 5))))
                )),
             (Exp_constant (Const_int 6))))
          )),
       (Exp_constant (Const_int 7))))
    |}]
;;

let%expect_test "test sum mult priority" =
  test_expr "4 + 6 * 7";
  [%expect
    {|
    (Exp_apply ((Exp_apply ((Exp_ident "( + )"), (Exp_constant (Const_int 4)))),
       (Exp_apply (
          (Exp_apply ((Exp_ident "( * )"), (Exp_constant (Const_int 6)))),
          (Exp_constant (Const_int 7))))
       ))
    |}]
;;

let%expect_test "test my operator" =
  test_expr "4 +== 5";
  [%expect
    {|
    (Exp_apply (
       (Exp_apply ((Exp_ident "( +== )"), (Exp_constant (Const_int 4)))),
       (Exp_constant (Const_int 5))))
    |}]
;;

let%expect_test "test right associativity" =
  test_expr "4 ** 5 ** 6";
  [%expect
    {|
    (Exp_apply ((Exp_apply ((Exp_ident "( ** )"), (Exp_constant (Const_int 4)))),
       (Exp_apply (
          (Exp_apply ((Exp_ident "( ** )"), (Exp_constant (Const_int 5)))),
          (Exp_constant (Const_int 6))))
       ))
    |}]
;;

let%expect_test "test unary prefix op" =
  test_expr "! 4";
  [%expect {| (Exp_apply ((Exp_ident "!"), (Exp_constant (Const_int 4)))) |}]
;;

let%expect_test "test several unary prefix op" =
  test_expr "! ! 4";
  [%expect
    {|
    (Exp_apply ((Exp_ident "!"),
       (Exp_apply ((Exp_ident "!"), (Exp_constant (Const_int 4))))))
    |}]
;;

let%expect_test "test parents priority" =
  test_expr "4 + (5 + 6)";
  [%expect
    {|
    (Exp_apply ((Exp_apply ((Exp_ident "( + )"), (Exp_constant (Const_int 4)))),
       (Exp_apply (
          (Exp_apply ((Exp_ident "( + )"), (Exp_constant (Const_int 5)))),
          (Exp_constant (Const_int 6))))
       ))
    |}]
;;

let%expect_test "test parents priority different operators" =
  test_expr "4 * (5 + 6)";
  [%expect
    {|
    (Exp_apply ((Exp_apply ((Exp_ident "( * )"), (Exp_constant (Const_int 4)))),
       (Exp_apply (
          (Exp_apply ((Exp_ident "( + )"), (Exp_constant (Const_int 5)))),
          (Exp_constant (Const_int 6))))
       ))
    |}]
;;

let%expect_test "test application" =
  test_expr "camel by camel";
  [%expect
    {|
    (Exp_apply ((Exp_apply ((Exp_ident "camel"), (Exp_ident "by"))),
       (Exp_ident "camel")))
    |}]
;;

let%expect_test "test prefix two args application" =
  test_expr "! 1 2";
  [%expect
    {|
    (Exp_apply ((Exp_apply ((Exp_ident "!"), (Exp_constant (Const_int 1)))),
       (Exp_constant (Const_int 2))))
    |}]
;;

let%expect_test "test prefix two args application" =
  test_expr "! ! 1 2 4";
  [%expect
    {|
    (Exp_apply (
       (Exp_apply (
          (Exp_apply ((Exp_ident "!"),
             (Exp_apply ((Exp_ident "!"), (Exp_constant (Const_int 1)))))),
          (Exp_constant (Const_int 2)))),
       (Exp_constant (Const_int 4))))
    |}]
;;

let%expect_test "test binary infix with prefix" =
  test_expr "2 + - 1 + 3";
  [%expect
    {|
    (Exp_apply (
       (Exp_apply ((Exp_ident "( + )"),
          (Exp_apply (
             (Exp_apply ((Exp_ident "( + )"), (Exp_constant (Const_int 2)))),
             (Exp_apply ((Exp_ident "-"), (Exp_constant (Const_int 1))))))
          )),
       (Exp_constant (Const_int 3))))
    |}]
;;

let%expect_test "test binary prefix call" =
  test_expr "(+) 2 3";
  [%expect
    {|
    (Exp_apply ((Exp_apply ((Exp_ident "+"), (Exp_constant (Const_int 2)))),
       (Exp_constant (Const_int 3))))
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
    (Exp_apply ((Exp_apply ((Exp_ident "( >>= )"), (Exp_ident "a"))),
       (Exp_apply ((Exp_apply ((Exp_ident "( ++ )"), (Exp_ident "b"))),
          (Exp_apply ((Exp_apply ((Exp_ident "( ** )"), (Exp_ident "c"))),
             (Exp_apply ((Exp_ident "d"),
                (Exp_apply ((Exp_ident "!+"), (Exp_ident "e")))))
             ))
          ))
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
         (Exp_apply (
            (Exp_apply ((Exp_ident "( + )"), (Exp_constant (Const_int 2)))),
            (Exp_constant (Const_int 4))))
         ])
    |}]
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
       [(Pat_binding ((Pat_constant (Const_int 4)), (Exp_constant (Const_int 4))
           ))
         ],
       (Exp_ident "b")))
     |}]
;;

let%expect_test "test let several" =
  test_expr "let rec a = 4 and b = 6 in a+b";
  [%expect
    {|
    (Exp_let (Recursive,
       [(Val_binding ("a", [], (Exp_constant (Const_int 4))));
         (Val_binding ("b", [], (Exp_constant (Const_int 6))))],
       (Exp_apply ((Exp_apply ((Exp_ident "( + )"), (Exp_ident "a"))),
          (Exp_ident "b")))
       ))
    |}]
;;

let%expect_test "test let in let" =
  test_expr "let a = let b = 4 in b in a";
  [%expect
    {|
    (Exp_let (Nonrecursive,
       [(Val_binding ("a", [],
           (Exp_let (Nonrecursive,
              [(Val_binding ("b", [], (Exp_constant (Const_int 4))))],
              (Exp_ident "b")))
           ))
         ],
       (Exp_ident "a")))
     |}]
;;

let%expect_test "test num + let" =
  test_expr "4 + let a = 5 in a";
  [%expect
    {|
    (Exp_apply ((Exp_apply ((Exp_ident "( + )"), (Exp_constant (Const_int 4)))),
       (Exp_let (Nonrecursive,
          [(Val_binding ("a", [], (Exp_constant (Const_int 5))))],
          (Exp_ident "a")))
       ))
    |}]
;;

let%expect_test "test fun" =
  test_expr "fun a b -> a+b";
  [%expect
    {|
    (Exp_fun ([(Pat_var "a"); (Pat_var "b")],
       (Exp_apply ((Exp_apply ((Exp_ident "( + )"), (Exp_ident "a"))),
          (Exp_ident "b")))
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
  [%expect
    {|
    (Exp_if ((Exp_ident "a"), (Exp_ident "b"), None))
     |}]
;;

let%expect_test "test if in expr" =
  test_expr "4 + if a then b else c";
  [%expect
    {|
    (Exp_apply ((Exp_apply ((Exp_ident "( + )"), (Exp_constant (Const_int 4)))),
       (Exp_if ((Exp_ident "a"), (Exp_ident "b"), (Some (Exp_ident "c"))))))
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
  [%expect
    {|
    (Exp_sequence (
       (Exp_if ((Exp_ident "a"), (Exp_ident "b"), (Some (Exp_ident "c")))),
       (Exp_ident "d")))
     |}]
;;

let%expect_test "test match" =
  test_expr "match 4 with|4 -> 1";
  [%expect
    {|
    (Exp_match ((Exp_constant (Const_int 4)),
       [{ left = (Pat_constant (Const_int 4));
          right = (Exp_constant (Const_int 1)) }
         ]
       ))
     |}]
;;

let%expect_test "test cons list" =
  test_expr "1 :: 2 :: []";
  [%expect
    {|
    (Exp_construct ("::",
       (Some (Exp_tuple
                [(Exp_constant (Const_int 1));
                  (Exp_construct ("::",
                     (Some (Exp_tuple
                              [(Exp_constant (Const_int 2));
                                (Exp_construct ("[]", None))]))
                     ))
                  ]))
       ))
     |}]
;;

let%expect_test "test list" =
  test_expr "[ 1; 2 ]";
  [%expect
    {|
    (Exp_construct ("::",
       (Some (Exp_tuple
                [(Exp_constant (Const_int 1));
                  (Exp_construct ("::",
                     (Some (Exp_tuple
                              [(Exp_constant (Const_int 2));
                                (Exp_construct ("[]", None))]))
                     ))
                  ]))
       ))
     |}]
;;

let%expect_test "test list inside" =
  test_expr "[ fun c -> c; fun b -> b ]";
  [%expect
    {|
    (Exp_construct ("::",
       (Some (Exp_tuple
                [(Exp_fun ([(Pat_var "c")],
                    (Exp_sequence ((Exp_ident "c"),
                       (Exp_fun ([(Pat_var "b")], (Exp_ident "b")))))
                    ));
                  (Exp_construct ("[]", None))]))
       ))
     |}]
;;

let%expect_test "test list inside parents" =
  test_expr "[ (fun c -> c); (fun b -> b) ]";
  [%expect
    {|
    (Exp_construct ("::",
       (Some (Exp_tuple
                [(Exp_fun ([(Pat_var "c")], (Exp_ident "c")));
                  (Exp_construct ("::",
                     (Some (Exp_tuple
                              [(Exp_fun ([(Pat_var "b")], (Exp_ident "b")));
                                (Exp_construct ("[]", None))]))
                     ))
                  ]))
       ))
     |}]
;;

let%expect_test "test list ops inside" =
  test_expr "[ 4 > 3; 3 > 2 ]";
  [%expect
    {|
    (Exp_construct ("::",
       (Some (Exp_tuple
                [(Exp_apply (
                    (Exp_apply ((Exp_ident "( > )"), (Exp_constant (Const_int 4))
                       )),
                    (Exp_constant (Const_int 3))));
                  (Exp_construct ("::",
                     (Some (Exp_tuple
                              [(Exp_apply (
                                  (Exp_apply ((Exp_ident "( > )"),
                                     (Exp_constant (Const_int 3)))),
                                  (Exp_constant (Const_int 2))));
                                (Exp_construct ("[]", None))]))
                     ))
                  ]))
       ))
    |}]
;;

let%expect_test "test typexpr" =
  test_expr "(4: int)";
  [%expect {| (Exp_type ((Exp_constant (Const_int 4)), (Type_single "int"))) |}]
;;

let%expect_test "test typexpr 2" =
  test_expr "(4 + 5: int)";
  [%expect
    {|
    (Exp_type (
       (Exp_apply (
          (Exp_apply ((Exp_ident "( + )"), (Exp_constant (Const_int 4)))),
          (Exp_constant (Const_int 5)))),
       (Type_single "int")))
    |}]
;;

let%expect_test "test typexpr fun" =
  test_expr "fun a:(int) -> a";
  [%expect
    {|
    (Exp_fun ([(Pat_var "a")], (Exp_type ((Exp_ident "a"), (Type_single "int")))
       )) |}]
;;

let%expect_test "test typexpr fun fun" =
  test_expr "fun a: (int -> int) -> a";
  [%expect
    {|
    (Exp_fun ([(Pat_var "a")],
       (Exp_type ((Exp_ident "a"),
          (Type_fun [(Type_single "int"); (Type_single "int")])))
       )) |}]
;;

let%expect_test "test typexpr fun arg" =
  test_expr "fun (a:int): int -> a";
  [%expect
    {|
    (Exp_fun ([(Pat_type ((Pat_var "a"), (Type_single "int")))],
       (Exp_type ((Exp_ident "a"), (Type_single "int"))))) |}]
;;

let%expect_test "test typexpr let" =
  test_expr "let a (b:int): int = b in a b";
  [%expect
    {|
    (Exp_let (Nonrecursive,
       [(Val_binding ("a", [(Pat_type ((Pat_var "b"), (Type_single "int")))],
           (Exp_type ((Exp_ident "b"), (Type_single "int")))))
         ],
       (Exp_apply ((Exp_ident "a"), (Exp_ident "b")))))
    |}]
;;

let%expect_test "test redefine operator" =
  test_expr "let (+) a b = a - b in let (!) m = m in a + !b";
  [%expect
    {|
    (Exp_let (Nonrecursive,
       [(Val_binding ("( + )", [(Pat_var "a"); (Pat_var "b")],
           (Exp_apply ((Exp_apply ((Exp_ident "( - )"), (Exp_ident "a"))),
              (Exp_ident "b")))
           ))
         ],
       (Exp_let (Nonrecursive,
          [(Val_binding ("( ! )", [(Pat_var "m")], (Exp_ident "m")))],
          (Exp_apply ((Exp_apply ((Exp_ident "( + )"), (Exp_ident "a"))),
             (Exp_apply ((Exp_ident "!"), (Exp_ident "b")))))
          ))
       ))
    |}]
;;

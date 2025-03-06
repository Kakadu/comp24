(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

let test_parser str =
  Test_utils.pp_result Parser.Ast.pp_structure @@ Parser.Main_parser.parse str
;;

let%expect_test "some beautiful test" =
  test_parser
    {|
let (+) a b = a - b
let m = 4
and mm = 6
m + let b = 6 in m + b
|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        [(Val_binding ("( + )", [(Pat_var "a"); (Pat_var "b")],
            (Exp_apply ((Exp_apply ((Exp_ident "( - )"), (Exp_ident "a"))),
               (Exp_ident "b")))
            ))
          ]
        ));
      (Str_value (Nonrecursive,
         [(Val_binding ("m", [], (Exp_constant (Const_int 4))));
           (Val_binding ("mm", [],
              (Exp_apply (
                 (Exp_apply ((Exp_ident "( + )"),
                    (Exp_apply ((Exp_constant (Const_int 6)), (Exp_ident "m"))))),
                 (Exp_let (Nonrecursive,
                    [(Val_binding ("b", [], (Exp_constant (Const_int 6))))],
                    (Exp_apply (
                       (Exp_apply ((Exp_ident "( + )"), (Exp_ident "m"))),
                       (Exp_ident "b")))
                    ))
                 ))
              ))
           ]
         ))
      ]
    |}]
;;

let%expect_test "let is an expression test" =
  test_parser "let a = 1 in a";
  [%expect
    {|

      [(Str_eval
          (Exp_let (Nonrecursive,
             [(Val_binding ("a", [], (Exp_constant (Const_int 1))))],
             (Exp_ident "a"))))
        ] |}]
;;

let%expect_test "let is not an expression test" =
  test_parser "let a = 1";
  [%expect
    {|

      [(Str_value (Nonrecursive,
          [(Val_binding ("a", [], (Exp_constant (Const_int 1))))]))
        ] |}]
;;

let%expect_test "Shrink of Sukharev test pattern" =
  test_parser {| let a = Some ( ( - ) 4 ) |};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        [(Val_binding ("a", [],
            (Exp_construct ("Some",
               (Some (Exp_apply ((Exp_ident "( - )"),
                        (Exp_constant (Const_int 4)))))
               ))
            ))
          ]
        ))
      ]
    |}]
;;

let%expect_test "Sukharev test pattern" =
  test_parser
    {| let _2 = let x, Some f = 1, Some ( ( + ) 4 ) in f x
 |};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        [(Val_binding ("_2", [],
            (Exp_let (Nonrecursive,
               [(Pat_binding (
                   (Pat_tuple
                      [(Pat_var "x");
                        (Pat_construct ("Some", (Some (Pat_var "f"))))]),
                   (Exp_tuple
                      [(Exp_constant (Const_int 1));
                        (Exp_construct ("Some",
                           (Some (Exp_apply ((Exp_ident "( + )"),
                                    (Exp_constant (Const_int 4)))))
                           ))
                        ])
                   ))
                 ],
               (Exp_apply ((Exp_ident "f"), (Exp_ident "x")))))
            ))
          ]
        ))
      ]
    |}]
;;

let%expect_test "Fun typing" =
  test_parser {| let a b:int = 5 + b|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        [(Val_binding ("a", [(Pat_var "b")],
            (Exp_type (
               (Exp_apply (
                  (Exp_apply ((Exp_ident "( + )"), (Exp_constant (Const_int 5)))),
                  (Exp_ident "b"))),
               (Type_single "int")))
            ))
          ]
        ))
      ]
    |}]
;;

let%expect_test "Constructor" =
  test_parser {|let H(a, b) = H(1, 2)|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        [(Pat_binding (
            (Pat_construct ("H",
               (Some (Pat_tuple [(Pat_var "a"); (Pat_var "b")])))),
            (Exp_construct ("H",
               (Some (Exp_tuple
                        [(Exp_constant (Const_int 1));
                          (Exp_constant (Const_int 2))]))
               ))
            ))
          ]
        ))
      ]
    |}]
;;

let%expect_test "infix +" =
  test_parser {|let a = + 4|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        [(Val_binding ("a", [],
            (Exp_apply ((Exp_ident "( ~+ )"), (Exp_constant (Const_int 4))))))
          ]
        ))
      ]
    |}]
;;

let%expect_test "one structure item" =
  test_parser
    {|
let m = 4
and mm = 6
m + let b = 6 in m + b
|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        [(Val_binding ("m", [], (Exp_constant (Const_int 4))));
          (Val_binding ("mm", [],
             (Exp_apply (
                (Exp_apply ((Exp_ident "( + )"),
                   (Exp_apply ((Exp_constant (Const_int 6)), (Exp_ident "m"))))),
                (Exp_let (Nonrecursive,
                   [(Val_binding ("b", [], (Exp_constant (Const_int 6))))],
                   (Exp_apply (
                      (Exp_apply ((Exp_ident "( + )"), (Exp_ident "m"))),
                      (Exp_ident "b")))
                   ))
                ))
             ))
          ]
        ))
      ]
    |}]
;;

let%expect_test "two str items" =
  test_parser
    {|
let m = 4
and mm = 6;;
m + let b = 6 in m + b
|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        [(Val_binding ("m", [], (Exp_constant (Const_int 4))));
          (Val_binding ("mm", [], (Exp_constant (Const_int 6))))]
        ));
      (Str_eval
         (Exp_apply ((Exp_apply ((Exp_ident "( + )"), (Exp_ident "m"))),
            (Exp_let (Nonrecursive,
               [(Val_binding ("b", [], (Exp_constant (Const_int 6))))],
               (Exp_apply ((Exp_apply ((Exp_ident "( + )"), (Exp_ident "m"))),
                  (Exp_ident "b")))
               ))
            )))
      ]
    |}]
;;

let%expect_test "list cons" =
  test_parser
    {|let [1; 2] :: [5] = [1; 2] :: [5]
|};
  [%expect
    {|
    [(Str_value (Nonrecursive,
        [(Pat_binding (
            (Pat_construct ("::",
               (Some (Pat_tuple
                        [(Pat_construct ("::",
                            (Some (Pat_tuple
                                     [(Pat_constant (Const_int 1));
                                       (Pat_construct ("::",
                                          (Some (Pat_tuple
                                                   [(Pat_constant (Const_int 2));
                                                     (Pat_construct ("[]", None))
                                                     ]))
                                          ))
                                       ]))
                            ));
                          (Pat_construct ("::",
                             (Some (Pat_tuple
                                      [(Pat_constant (Const_int 5));
                                        (Pat_construct ("[]", None))]))
                             ))
                          ]))
               )),
            (Exp_construct ("::",
               (Some (Exp_tuple
                        [(Exp_construct ("::",
                            (Some (Exp_tuple
                                     [(Exp_constant (Const_int 1));
                                       (Exp_construct ("::",
                                          (Some (Exp_tuple
                                                   [(Exp_constant (Const_int 2));
                                                     (Exp_construct ("[]", None))
                                                     ]))
                                          ))
                                       ]))
                            ));
                          (Exp_construct ("::",
                             (Some (Exp_tuple
                                      [(Exp_constant (Const_int 5));
                                        (Exp_construct ("[]", None))]))
                             ))
                          ]))
               ))
            ))
          ]
        ))
      ]
    |}]
;;

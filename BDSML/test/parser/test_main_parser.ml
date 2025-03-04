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

let%expect_test "some beautiful test with semicolon" =
  test_parser
    {|
let (+) a b = a - b;;
let m = 4
and mm = 6;;
m + let b = 6 in m + b ;;;;
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
  [%expect {||}]
;;

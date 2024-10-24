(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

let test_parser str =
  Test_utils.pp_result Parser.Ast.pp_structure @@ Parser.Main_parser.parse str
;;

let%expect_test "some beautiful test" =
  test_parser {|
let (+) a b = a - b
let m = 4
and mm = 6
m + let b = 6 in m + b
|};
  [%expect {|
    [(Str_value (Nonrecursive,
        [(Val_binding ("+", [(Pat_var "a"); (Pat_var "b")],
            (Exp_apply ((Exp_ident "-"),
               (Exp_tuple [(Exp_ident "a"); (Exp_ident "b")])))
            ))
          ]
        ));
      (Str_value (Nonrecursive,
         [(Pat_binding ((Pat_var "m"), (Exp_constant (Const_int 4))));
           (Pat_binding ((Pat_var "mm"), (Exp_constant (Const_int 6))))]
         ));
      (Str_eval
         (Exp_apply ((Exp_ident "+"),
            (Exp_tuple
               [(Exp_ident "m");
                 (Exp_let (Nonrecursive,
                    [(Pat_binding ((Pat_var "b"), (Exp_constant (Const_int 6))))],
                    (Exp_apply ((Exp_ident "+"),
                       (Exp_tuple [(Exp_ident "m"); (Exp_ident "b")])))
                    ))
                 ])
            )))
      ] |}]
;;

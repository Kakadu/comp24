(** Copyright 2024, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Test_utils

let test_pattern str =
  Test_utils.pp_result Parser.Ast.pp_pattern
  @@ parse_with_parser Parser.Pattern_parser.parse_pattern str
;;

let%expect_test "test any" =
  test_pattern "_";
  [%expect {| Pat_any |}]
;;

let%expect_test "test const int" =
  test_pattern "   5";
  [%expect {| (Pat_constant (Const_int 5)) |}]
;;

let%expect_test "test const string" =
  test_pattern " \"abraacadabra\"";
  [%expect {| (Pat_constant (Const_string "abraacadabra")) |}]
;;

let%expect_test "test const char" =
  test_pattern " (((\'Z\')))";
  [%expect {| (Pat_constant (Const_char 'Z')) |}]
;;

let%expect_test "test const char" =
  test_pattern " \'Z\' | \'V\'";
  [%expect
    {| (Pat_or ((Pat_constant (Const_char 'Z')), (Pat_constant (Const_char 'V')))) |}]
;;

let%expect_test "test tuple" =
  test_pattern "(((1), 2, (((3))), ((4))))";
  [%expect
    {|
    (Pat_tuple
       [(Pat_constant (Const_int 1)); (Pat_constant (Const_int 2));
         (Pat_constant (Const_int 3)); (Pat_constant (Const_int 4))])|}]
;;

let%expect_test "test list 1" =
  test_pattern "[1; 2; 3]";
  [%expect
    {|
    (Pat_construct ("::",
       (Some (Pat_tuple
                [(Pat_constant (Const_int 1));
                  (Pat_construct ("::",
                     (Some (Pat_tuple
                              [(Pat_constant (Const_int 2));
                                (Pat_construct ("::",
                                   (Some (Pat_tuple
                                            [(Pat_constant (Const_int 3));
                                              (Pat_construct ("[]", None))]))
                                   ))
                                ]))
                     ))
                  ]))
       ))
    |}]
;;

let%expect_test "test list 2" =
  test_pattern "((1) :: (2) :: [])";
  [%expect
    {|
    (Pat_construct ("::",
       (Some (Pat_tuple
                [(Pat_constant (Const_int 1));
                  (Pat_construct ("::",
                     (Some (Pat_tuple
                              [(Pat_constant (Const_int 2));
                                (Pat_construct ("[]", None))]))
                     ))
                  ]))
       ))
    |}]
;;

let%expect_test "test list 3" =
  test_pattern "[ [1] ]";
  [%expect
    {|
    (Pat_construct ("::",
       (Some (Pat_tuple
                [(Pat_construct ("::",
                    (Some (Pat_tuple
                             [(Pat_constant (Const_int 1));
                               (Pat_construct ("[]", None))]))
                    ));
                  (Pat_construct ("[]", None))]))
       ))
    |}]
;;

let%expect_test "test list 4" =
  test_pattern "_ :: [[2; 4]; [3]]";
  [%expect
    {|
    (Pat_construct ("::",
       (Some (Pat_tuple
                [Pat_any;
                  (Pat_construct ("::",
                     (Some (Pat_tuple
                              [(Pat_construct ("::",
                                  (Some (Pat_tuple
                                           [(Pat_constant (Const_int 2));
                                             (Pat_construct ("::",
                                                (Some (Pat_tuple
                                                         [(Pat_constant
                                                             (Const_int 4));
                                                           (Pat_construct ("[]",
                                                              None))
                                                           ]))
                                                ))
                                             ]))
                                  ));
                                (Pat_construct ("::",
                                   (Some (Pat_tuple
                                            [(Pat_construct ("::",
                                                (Some (Pat_tuple
                                                         [(Pat_constant
                                                             (Const_int 3));
                                                           (Pat_construct ("[]",
                                                              None))
                                                           ]))
                                                ));
                                              (Pat_construct ("[]", None))]))
                                   ))
                                ]))
                     ))
                  ]))
       ))
    |}]
;;

let%expect_test "or test" =
  test_pattern "1 | 2 :: 1, 2, 3";
  [%expect
    {|
    (Pat_or ((Pat_constant (Const_int 1)),
       (Pat_tuple
          [(Pat_construct ("::",
              (Some (Pat_tuple
                       [(Pat_constant (Const_int 2));
                         (Pat_constant (Const_int 1))]))
              ));
            (Pat_constant (Const_int 2)); (Pat_constant (Const_int 3))])
       ))
    |}]
;;

let%expect_test "or inside of list test" =
  test_pattern "[1|2; 3]";
  [%expect
    {|
    (Pat_construct ("::",
       (Some (Pat_tuple
                [(Pat_or ((Pat_constant (Const_int 1)),
                    (Pat_constant (Const_int 2))));
                  (Pat_construct ("::",
                     (Some (Pat_tuple
                              [(Pat_constant (Const_int 3));
                                (Pat_construct ("[]", None))]))
                     ))
                  ]))
       ))
    |}]
;;

let%expect_test "var" =
  test_pattern "a";
  [%expect {| (Pat_var "a") |}]
;;

let%expect_test "var hard" =
  test_pattern "[a | b; c]";
  [%expect
    {|
    (Pat_construct ("::",
       (Some (Pat_tuple
                [(Pat_or ((Pat_var "a"), (Pat_var "b")));
                  (Pat_construct ("::",
                     (Some (Pat_tuple
                              [(Pat_var "c"); (Pat_construct ("[]", None))]))
                     ))
                  ]))
       )) |}]
;;

let%expect_test "pat type" =
  test_pattern "([a]: int list)";
  [%expect
    {|
    (Pat_type (
       (Pat_construct ("::",
          (Some (Pat_tuple [(Pat_var "a"); (Pat_construct ("[]", None))])))),
       (Type_constructor_param ((Type_single "int"), "list")))) |}]
;;

let%expect_test "pat construct" =
  test_pattern "Some";
  [%expect
    {|
    (Pat_construct ("Some", None)) |}]
;;

let%expect_test "pat construct args" =
  test_pattern "Some true";
  [%expect
    {|
    (Pat_construct ("Some", (Some (Pat_constant (Const_bool true))))) |}]
;;

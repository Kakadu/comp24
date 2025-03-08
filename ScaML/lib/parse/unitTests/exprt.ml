(** Copyright 2024-2025, Danil Slinchuk, Julia Kononova *)

(** SPDX-License-Identifier: MIT *)

open! Base
open Ast
open Pp
open Expr

let%expect_test "parse_function_pattern_matching" =
  pp pp_expression parse_expression "function | a -> true | b -> false" ;
  [%expect {|
       syntax error |}]

let%expect_test "parse_lambda_fun" =
  pp pp_expression parse_expression "fun x y -> (x + y : int)" ;
  [%expect
    {|
       (Exp_fun (((Pat_var (Ident "x")), [(Pat_var (Ident "y"))]),
          (Exp_constraint (
             (Exp_apply (
                (Exp_apply ((Exp_ident (Ident "+")), (Exp_ident (Ident "x")))),
                (Exp_ident (Ident "y")))),
             int))
          )) |}]

let%expect_test "parse_custom_operator" =
  pp pp_expression parse_expression "a >>= b ++ c ** d !+ e" ;
  [%expect
    {|
       (Exp_apply ((Exp_apply ((Exp_ident (Ident ">>=")), (Exp_ident (Ident "a")))),
          (Exp_apply (
             (Exp_apply ((Exp_ident (Ident "++")), (Exp_ident (Ident "b")))),
             (Exp_apply (
                (Exp_apply ((Exp_ident (Ident "**")), (Exp_ident (Ident "c")))),
                (Exp_apply ((Exp_ident (Ident "d")),
                   (Exp_apply ((Exp_ident (Ident "!+")), (Exp_ident (Ident "e"))))))
                ))
             ))
          )) |}]

let%expect_test "parse_let" =
  pp pp_expression parse_expression "let rec a = 1 and b = 2 in let e = 3 in a" ;
  [%expect
    {|
       (Exp_let (Recursive,
          ({ pat = (Pat_var (Ident "a")); expr = (Exp_constant (Const_integer 1)) },
           [{ pat = (Pat_var (Ident "b")); expr = (Exp_constant (Const_integer 2)) }
             ]),
          (Exp_let (Nonrecursive,
             ({ pat = (Pat_var (Ident "e")); expr = (Exp_constant (Const_integer 3))
                },
              []),
             (Exp_ident (Ident "a"))))
          )) |}]

let%expect_test "parse_ifthenelse" =
  pp pp_expression parse_expression "if a then (if b then c) else d" ;
  [%expect
    {|
       (Exp_ifthenelse ((Exp_ident (Ident "a")),
          (Exp_ifthenelse ((Exp_ident (Ident "b")), (Exp_ident (Ident "c")), None)),
          (Some (Exp_ident (Ident "d"))))) |}]

let%expect_test "parse_match1" =
  pp pp_expression parse_expression "match a with b -> c | d -> e" ;
  [%expect
    {|
       (Exp_match ((Exp_ident (Ident "a")),
          ({ left = (Pat_var (Ident "b")); right = (Exp_ident (Ident "c")) },
           [{ left = (Pat_var (Ident "d")); right = (Exp_ident (Ident "e")) }])
          )) |}]

let%expect_test "parse_match2" =
  pp pp_expression parse_expression "match a with | b | c | d -> e | f -> g" ;
  [%expect {|
       syntax error |}]

let%expect_test "parse_constr1" =
  pp pp_expression parse_expression "Nil" ;
  [%expect {| (Exp_construct ((Ident "Nil"), None)) |}]

let%expect_test "parse_constr2" =
  pp pp_expression parse_expression "Some x" ;
  [%expect
    {| (Exp_construct ((Ident "Some"), (Some (Exp_ident (Ident "x"))))) |}]

let%expect_test "parse_constr3" =
  pp pp_expression parse_expression "Cons (1, Nil)" ;
  [%expect
    {|
       (Exp_construct ((Ident "Cons"),
          (Some (Exp_tuple
                   ((Exp_constant (Const_integer 1)),
                    (Exp_construct ((Ident "Nil"), None)), [])))
          )) |}]

let%expect_test "parse_list" =
  pp pp_expression parse_expression "[a;b;c]" ;
  [%expect
    {|
       (Exp_construct ((Ident "::"),
          (Some (Exp_tuple
                   ((Exp_ident (Ident "a")),
                    (Exp_construct ((Ident "::"),
                       (Some (Exp_tuple
                                ((Exp_ident (Ident "b")),
                                 (Exp_construct ((Ident "::"),
                                    (Some (Exp_tuple
                                             ((Exp_ident (Ident "c")),
                                              (Exp_construct ((Ident "[]"), None)),
                                              [])))
                                    )),
                                 [])))
                       )),
                    [])))
          )) |}]

let%expect_test "parse_list_1element" =
  pp pp_expression parse_expression "[a]" ;
  [%expect
    {|
       (Exp_construct ((Ident "::"),
          (Some (Exp_tuple
                   ((Exp_ident (Ident "a")), (Exp_construct ((Ident "[]"), None)),
                    [])))
          )) |}]

let%expect_test "parse_list_empty" =
  pp pp_expression parse_expression "[]" ;
  [%expect {| (Exp_construct ((Ident "[]"), None)) |}]

let%expect_test "parse_list_op" =
  pp pp_expression parse_expression "(a :: b) :: c :: d :: []" ;
  [%expect
    {|
       (Exp_construct ((Ident "::"),
          (Some (Exp_tuple
                   ((Exp_construct ((Ident "::"),
                       (Some (Exp_tuple
                                ((Exp_ident (Ident "a")), (Exp_ident (Ident "b")),
                                 [])))
                       )),
                    (Exp_construct ((Ident "::"),
                       (Some (Exp_tuple
                                ((Exp_ident (Ident "c")),
                                 (Exp_construct ((Ident "::"),
                                    (Some (Exp_tuple
                                             ((Exp_ident (Ident "d")),
                                              (Exp_construct ((Ident "[]"), None)),
                                              [])))
                                    )),
                                 [])))
                       )),
                    [])))
          )) |}]

let%expect_test "parse_tuple_op" =
  pp pp_expression parse_expression "a, (b, c), d, e" ;
  [%expect
    {|
       (Exp_tuple
          ((Exp_ident (Ident "a")),
           (Exp_tuple ((Exp_ident (Ident "b")), (Exp_ident (Ident "c")), [])),
           [(Exp_ident (Ident "d")); (Exp_ident (Ident "e"))])) |}]

let%expect_test "parse_plus_minus_prefix_op" =
  pp pp_expression parse_expression "1 + - + + 3" ;
  [%expect
    {|
       (Exp_apply (
          (Exp_apply ((Exp_ident (Ident "+")), (Exp_constant (Const_integer 1)))),
          (Exp_apply ((Exp_ident (Ident "~-")),
             (Exp_apply ((Exp_ident (Ident "~+")),
                (Exp_apply ((Exp_ident (Ident "~+")),
                   (Exp_constant (Const_integer 3))))
                ))
             ))
          )) |}]

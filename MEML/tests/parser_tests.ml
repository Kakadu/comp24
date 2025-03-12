(** Copyright 2024-2025, Perevalov Efim, Ermolovich Anna *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open MEML_lib
open Ast
open Parser

(* TESTS  PARSER*)

let start_test parser show input =
  let res = start_parsing parser input in
  match res with
  | Ok res -> Format.printf "%s" (show res)
  | Error err -> Format.printf "%s" err
;;

(* Test pattern parser *)

let%expect_test "pattern test" =
  let test = "true" in
  start_test parse_pattern show_pattern test;
  [%expect {| (PConst (CBool true)) |}]
;;

let%expect_test "pattern test" =
  let test = "951753" in
  start_test parse_pattern show_pattern test;
  [%expect {| (PConst (CInt 951753)) |}]
;;

let%expect_test "pattern test" =
  let test = "var" in
  start_test parse_pattern show_pattern test;
  [%expect {| (PVar ("var", TUnknown)) |}]
;;

let%expect_test "pattern test" =
  let test = "(var : int)" in
  start_test parse_pattern show_pattern test;
  [%expect {| (PVar ("var", TInt)) |}]
;;

let%expect_test "pattern test" =
  let test = "((1, 2), (a: int), true)" in
  start_test parse_pattern show_pattern test;
  [%expect
    {|
    (PTuple
       [(PTuple [(PConst (CInt 1)); (PConst (CInt 2))]); (PVar ("a", TInt));
         (PConst (CBool true))]) |}]
;;

let%expect_test "pattern test" =
  let test = "[1 :: (a: int); [1; 2]; (1, 2, 3); true]" in
  start_test parse_pattern show_pattern test;
  [%expect
    {|
    (PCon ((PCon ((PConst (CInt 1)), (PVar ("a", TInt)))),
       (PCon ((PCon ((PConst (CInt 1)), (PConst (CInt 2)))),
          (PCon (
             (PTuple [(PConst (CInt 1)); (PConst (CInt 2)); (PConst (CInt 3))]),
             (PConst (CBool true))))
          ))
       )) |}]
;;

(* Test expression *)

(* EConst *)

let%expect_test "expression_test" =
  let test = "1" in
  start_test parse_expression show_expression test;
  [%expect {| (EConst (CInt 1)) |}]
;;

let%expect_test "expression_test" =
  let test = "false" in
  start_test parse_expression show_expression test;
  [%expect {| (EConst (CBool false)) |}]
;;

(* EVar *)

let%expect_test "expression_test" =
  let test = "papa" in
  start_test parse_expression show_expression test;
  [%expect {| (EVar ("papa", TUnknown)) |}]
;;

let%expect_test "expression_test" =
  let test = "(papa : int)" in
  start_test parse_expression show_expression test;
  [%expect {| (EVar ("papa", TInt)) |}]
;;

let%expect_test "expression_test" =
  let test = "([1; ([2; ([3; 4], 5)], 6)], 7)" in
  start_test parse_expression show_expression test;
  [%expect
    {|
    (ETuple
       [(EList ((EConst (CInt 1)),
           (EList (
              (ETuple
                 [(EList ((EConst (CInt 2)),
                     (EList (
                        (ETuple
                           [(EList ((EConst (CInt 3)),
                               (EList ((EConst (CInt 4)), (EConst CNil)))));
                             (EConst (CInt 5))]),
                        (EConst CNil)))
                     ));
                   (EConst (CInt 6))]),
              (EConst CNil)))
           ));
         (EConst (CInt 7))]) |}]
;;

let%expect_test "expression_test" =
  let test = "[1; 5]" in
  start_test parse_expression show_expression test;
  [%expect {| (EList ((EConst (CInt 1)), (EList ((EConst (CInt 5)), (EConst CNil))))) |}]
;;

(* EBinaryOp *)

let%expect_test "expression_test" =
  let test = "1 + (a * 3) - x" in
  start_test parse_expression show_expression test;
  [%expect
    {|
    (EBinaryOp (Sub,
       (EBinaryOp (Add, (EConst (CInt 1)),
          (EBinaryOp (Mul, (EVar ("a", TUnknown)), (EConst (CInt 3)))))),
       (EVar ("x", TUnknown)))) |}]
;;

let%expect_test "expression_test" =
  let test = "true || false" in
  start_test parse_expression show_expression test;
  [%expect {| (EBinaryOp (Or, (EConst (CBool true)), (EConst (CBool false)))) |}]
;;

(* EIfElse *)

let%expect_test "expression_test" =
  let test = "if n = 1 then 1 else 3" in
  start_test parse_expression show_expression test;
  [%expect
    {|
    (EIfElse ((EBinaryOp (Eq, (EVar ("n", TUnknown)), (EConst (CInt 1)))),
       (EConst (CInt 1)), (EConst (CInt 3)))) |}]
;;

(* EFun *)

let%expect_test "expression_test" =
  let test = "fun x -> 3 + x" in
  start_test parse_expression show_expression test;
  [%expect
    {|
    (EFun ((PVar ("x", TUnknown)),
       (EBinaryOp (Add, (EConst (CInt 3)), (EVar ("x", TUnknown)))))) |}]
;;

let%expect_test "expression_test" =
  let test = "fun x y -> y + x" in
  start_test parse_expression show_expression test;
  [%expect
    {|
    (EFun ((PVar ("x", TUnknown)),
       (EFun ((PVar ("y", TUnknown)),
          (EBinaryOp (Add, (EVar ("y", TUnknown)), (EVar ("x", TUnknown))))))
       )) |}]
;;

(* EApp *)

let%expect_test "expression_test" =
  let test = "is_something yes no" in
  start_test parse_expression show_expression test;
  [%expect
    {|
    (EApp (
       (EApp ((EVar ("is_something", TUnknown)), (EVar ("yes", TUnknown)),
          TUnknown)),
       (EVar ("no", TUnknown)), TUnknown)) |}]
;;

let%expect_test "expression_test" =
  let test = "(fun x -> x + 1) 1" in
  start_test parse_expression show_expression test;
  [%expect
    {|
    (EApp (
       (EFun ((PVar ("x", TUnknown)),
          (EBinaryOp (Add, (EVar ("x", TUnknown)), (EConst (CInt 1)))))),
       (EConst (CInt 1)), TUnknown)) |}]
;;

(* ELetIn *)

let%expect_test "expression_test" =
  let test = "let sum a b = a + b in sum 2 3" in
  start_test parse_expression show_expression test;
  [%expect
    {|
    (ELetIn (Notrec, "sum",
       (EFun ((PVar ("a", TUnknown)),
          (EFun ((PVar ("b", TUnknown)),
             (EBinaryOp (Add, (EVar ("a", TUnknown)), (EVar ("b", TUnknown))))))
          )),
       (EApp ((EApp ((EVar ("sum", TUnknown)), (EConst (CInt 2)), TUnknown)),
          (EConst (CInt 3)), TUnknown))
       )) |}]
;;

(**  List and tuple *)

let%expect_test "expression_test" =
  let test = "[1;2;3;4]" in
  start_test parse_expression show_expression test;
  [%expect
    {|
    (EList ((EConst (CInt 1)),
       (EList ((EConst (CInt 2)),
          (EList ((EConst (CInt 3)), (EList ((EConst (CInt 4)), (EConst CNil)))))
          ))
       ))
 |}]
;;

let%expect_test "expression_test" =
  let test = "(1,2,3,4,5)" in
  start_test parse_expression show_expression test;
  [%expect
    {|
    (ETuple
       [(EConst (CInt 1)); (EConst (CInt 2)); (EConst (CInt 3));
         (EConst (CInt 4)); (EConst (CInt 5))])
 |}]
;;

(** Test bindings *)

let%expect_test "bindings_test" =
  let test = "let plusfive x = let five a = a + 5 in five x" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec,
       [((PVar ("plusfive", TUnknown)),
         (EFun ((PVar ("x", TUnknown)),
            (ELetIn (Notrec, "five",
               (EFun ((PVar ("a", TUnknown)),
                  (EBinaryOp (Add, (EVar ("a", TUnknown)), (EConst (CInt 5)))))),
               (EApp ((EVar ("five", TUnknown)), (EVar ("x", TUnknown)), TUnknown
                  ))
               ))
            )))
         ]
       )) |}]
;;

let%expect_test "bindings_test" =
  let test = "4 + 3" in
  start_test parse_bindings show_bindings test;
  [%expect {|
    (Expression (EBinaryOp (Add, (EConst (CInt 4)), (EConst (CInt 3))))) |}]
;;

let%expect_test "bindings_test" =
  let test = "let (+) x y = x - y" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec,
       [((PVar ("Add", (TArrow (TInt, (TArrow (TInt, TInt)))))),
         (EFun ((PVar ("x", TUnknown)),
            (EFun ((PVar ("y", TUnknown)),
               (EBinaryOp (Sub, (EVar ("x", TUnknown)), (EVar ("y", TUnknown))))
               ))
            )))
         ]
       ))|}]
;;

let%expect_test "bindings_test" =
  let test = "let f (x: int) (y: int) = x + y" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec,
       [((PVar ("f", TUnknown)),
         (EFun ((PVar ("x", TInt)),
            (EFun ((PVar ("y", TInt)),
               (EBinaryOp (Add, (EVar ("x", TUnknown)), (EVar ("y", TUnknown))))
               ))
            )))
         ]
       ))
 |}]
;;

let%expect_test "bindings_test" =
  let test = "[1;2;3;4]" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Expression
       (EList ((EConst (CInt 1)),
          (EList ((EConst (CInt 2)),
             (EList ((EConst (CInt 3)),
                (EList ((EConst (CInt 4)), (EConst CNil)))))
             ))
          )))
 |}]
;;

let%expect_test "bindings_test" =
  let test = "(1,2,3,4,5)" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Expression
       (ETuple
          [(EConst (CInt 1)); (EConst (CInt 2)); (EConst (CInt 3));
            (EConst (CInt 4)); (EConst (CInt 5))]))
 |}]
;;

let%expect_test "bindings_test" =
  let test = "let f (x: int) = x + 4" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec,
       [((PVar ("f", TUnknown)),
         (EFun ((PVar ("x", TInt)),
            (EBinaryOp (Add, (EVar ("x", TUnknown)), (EConst (CInt 4)))))))
         ]
       ))
 |}]
;;

let%expect_test "bindings_test" =
  let test = "2 * (4 + 4) * 1" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Expression
       (EBinaryOp (Mul,
          (EBinaryOp (Mul, (EConst (CInt 2)),
             (EBinaryOp (Add, (EConst (CInt 4)), (EConst (CInt 4)))))),
          (EConst (CInt 1)))))
 |}]
;;

let%expect_test "bindings_test" =
  let test =
    "let rec fib n = \n\
    \      if n < 1 \n\
    \        then 1 \n\
    \      else fib (n - 1) + fib (n - 2)"
  in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Rec,
       [((PVar ("fib", TUnknown)),
         (EFun ((PVar ("n", TUnknown)),
            (EIfElse (
               (EBinaryOp (Less, (EVar ("n", TUnknown)), (EConst (CInt 1)))),
               (EConst (CInt 1)),
               (EBinaryOp (Add,
                  (EApp ((EVar ("fib", TUnknown)),
                     (EBinaryOp (Sub, (EVar ("n", TUnknown)), (EConst (CInt 1)))),
                     TUnknown)),
                  (EApp ((EVar ("fib", TUnknown)),
                     (EBinaryOp (Sub, (EVar ("n", TUnknown)), (EConst (CInt 2)))),
                     TUnknown))
                  ))
               ))
            )))
         ]
       ))
 |}]
;;

let%expect_test "bindings_test" =
  let test =
    "let rec cps_fact x = \n\
    \      let helper x acc = \n\
    \        if x = 1 \n\
    \          then acc x \n\
    \        else helper (x - 1) (fun n -> n * acc x) \n\
    \      in \n\
    \      helper x (fun a -> a)"
  in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Rec,
       [((PVar ("cps_fact", TUnknown)),
         (EFun ((PVar ("x", TUnknown)),
            (ELetIn (Notrec, "helper",
               (EFun ((PVar ("x", TUnknown)),
                  (EFun ((PVar ("acc", TUnknown)),
                     (EIfElse (
                        (EBinaryOp (Eq, (EVar ("x", TUnknown)), (EConst (CInt 1))
                           )),
                        (EApp ((EVar ("acc", TUnknown)), (EVar ("x", TUnknown)),
                           TUnknown)),
                        (EApp (
                           (EApp ((EVar ("helper", TUnknown)),
                              (EBinaryOp (Sub, (EVar ("x", TUnknown)),
                                 (EConst (CInt 1)))),
                              TUnknown)),
                           (EFun ((PVar ("n", TUnknown)),
                              (EBinaryOp (Mul, (EVar ("n", TUnknown)),
                                 (EApp ((EVar ("acc", TUnknown)),
                                    (EVar ("x", TUnknown)), TUnknown))
                                 ))
                              )),
                           TUnknown))
                        ))
                     ))
                  )),
               (EApp (
                  (EApp ((EVar ("helper", TUnknown)), (EVar ("x", TUnknown)),
                     TUnknown)),
                  (EFun ((PVar ("a", TUnknown)), (EVar ("a", TUnknown)))),
                  TUnknown))
               ))
            )))
         ]
       ))
 |}]
;;

let%expect_test "bindings_test" =
  let test = "let f = fun (g: int -> int -> int) (x: int) (y: int) -> g x y" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec,
       [((PVar ("f", TUnknown)),
         (EFun ((PVar ("g", (TArrow (TInt, (TArrow (TInt, TInt)))))),
            (EFun ((PVar ("x", TInt)),
               (EFun ((PVar ("y", TInt)),
                  (EApp (
                     (EApp ((EVar ("g", TUnknown)), (EVar ("x", TUnknown)),
                        TUnknown)),
                     (EVar ("y", TUnknown)), TUnknown))
                  ))
               ))
            )))
         ]
       ))
 |}]
;;

let%expect_test "bindings_test" =
  let test = "let (+) = fun (x: int) (y: int) -> x - y" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec,
       [((PVar ("Add", (TArrow (TInt, (TArrow (TInt, TInt)))))),
         (EFun ((PVar ("x", TInt)),
            (EFun ((PVar ("y", TInt)),
               (EBinaryOp (Sub, (EVar ("x", TUnknown)), (EVar ("y", TUnknown))))
               ))
            )))
         ]
       ))
 |}]
;;

let%expect_test "bindings_test" =
  let test = "let h f1 f2 f3 f4 f5 f6 f7 = f1 (f2 (f3 (f4 (f5 (f6 f7)))))" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec,
       [((PVar ("h", TUnknown)),
         (EFun ((PVar ("f1", TUnknown)),
            (EFun ((PVar ("f2", TUnknown)),
               (EFun ((PVar ("f3", TUnknown)),
                  (EFun ((PVar ("f4", TUnknown)),
                     (EFun ((PVar ("f5", TUnknown)),
                        (EFun ((PVar ("f6", TUnknown)),
                           (EFun ((PVar ("f7", TUnknown)),
                              (EApp ((EVar ("f1", TUnknown)),
                                 (EApp ((EVar ("f2", TUnknown)),
                                    (EApp ((EVar ("f3", TUnknown)),
                                       (EApp ((EVar ("f4", TUnknown)),
                                          (EApp ((EVar ("f5", TUnknown)),
                                             (EApp ((EVar ("f6", TUnknown)),
                                                (EVar ("f7", TUnknown)), TUnknown
                                                )),
                                             TUnknown)),
                                          TUnknown)),
                                       TUnknown)),
                                    TUnknown)),
                                 TUnknown))
                              ))
                           ))
                        ))
                     ))
                  ))
               ))
            )))
         ]
       ))
 |}]
;;

let%expect_test "bindings_test" =
  let test = "let h f1 f2 f3 f4 f5 f6 f7 = ((((((f1 f2) f3) f4) f5) f6) f7)" in
  start_test parse_bindings show_bindings test;
  [%expect
    {|
    (Let (Notrec,
       [((PVar ("h", TUnknown)),
         (EFun ((PVar ("f1", TUnknown)),
            (EFun ((PVar ("f2", TUnknown)),
               (EFun ((PVar ("f3", TUnknown)),
                  (EFun ((PVar ("f4", TUnknown)),
                     (EFun ((PVar ("f5", TUnknown)),
                        (EFun ((PVar ("f6", TUnknown)),
                           (EFun ((PVar ("f7", TUnknown)),
                              (EApp (
                                 (EApp (
                                    (EApp (
                                       (EApp (
                                          (EApp (
                                             (EApp ((EVar ("f1", TUnknown)),
                                                (EVar ("f2", TUnknown)), TUnknown
                                                )),
                                             (EVar ("f3", TUnknown)), TUnknown)),
                                          (EVar ("f4", TUnknown)), TUnknown)),
                                       (EVar ("f5", TUnknown)), TUnknown)),
                                    (EVar ("f6", TUnknown)), TUnknown)),
                                 (EVar ("f7", TUnknown)), TUnknown))
                              ))
                           ))
                        ))
                     ))
                  ))
               ))
            )))
         ]
       ))
 |}]
;;

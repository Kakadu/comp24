(** Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Roflanml_lib
open Ast

module ParserTests = struct
  open Parser

  let pp printer parser input =
    match parser input with
    | Result.Ok res -> Stdlib.Format.printf "%a" printer res
    | _ -> Stdlib.print_endline "Failed to parse"
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "let f x = x";
    [%expect {| (ELet (NonRec, "f", (EFun ("x", (EVar "x"))), None)) |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "if f x then x else 0";
    [%expect
      {| (EBranch ((EApp ((EVar "f"), (EVar "x"))), (EVar "x"), (EConst (CInt 0)))) |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "[1 + 2; 1 * 2; 1 - 2; 1 / 2]";
    [%expect
      {| 
    (EList
       [(EBinop (Add, (EConst (CInt 1)), (EConst (CInt 2))));
         (EBinop (Mul, (EConst (CInt 1)), (EConst (CInt 2))));
         (EBinop (Sub, (EConst (CInt 1)), (EConst (CInt 2))));
         (EBinop (Div, (EConst (CInt 1)), (EConst (CInt 2))))]) 
    |}]
  ;;

  let%expect_test _ =
    pp
      pp_expr
      parse_expr
      {|match x with | h1 :: h2 :: tl -> if h1 >= h2 then h1 else h2 | h1 :: [] -> h1 | _ -> 0|};
    [%expect
      {|
        (EMatch ((EVar "x"),
           [((PCons ((PVar "h1"), (PVar "h2"), [(PVar "tl")])),
             (EBranch ((EBinop (Geq, (EVar "h1"), (EVar "h2"))), (EVar "h1"),
                (EVar "h2"))));
             ((PCons ((PVar "h1"), PEmpty, [])), (EVar "h1"));
             (PWild, (EConst (CInt 0)))]
           )) 
    |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "[1; 2], [3; 4]";
    [%expect
      {|
    (ETuple ((EList [(EConst (CInt 1)); (EConst (CInt 2))]),
       (EList [(EConst (CInt 3)); (EConst (CInt 4))]), [])) 
    |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "[1, 2, 3; 4, 5, 6]";
    [%expect
      {|
    (EList
       [(ETuple ((EConst (CInt 1)), (EConst (CInt 2)), [(EConst (CInt 3))]));
         (ETuple ((EConst (CInt 4)), (EConst (CInt 5)), [(EConst (CInt 6))]))]) 
    |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "let () = () in ()";
    [%expect {| (ELet (NonRec, "()", (EConst CUnit), (Some (EConst CUnit)))) |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "let f (x : bool) (y: int)= x * y";
    [%expect
      {|
    (ELet (NonRec, [("f", TUnit); ("x", TBool); ("y", TInt)],
       (EBinop (Mul, (EVar "x"), (EVar "y"))), None))  |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "let (+) = fun (x: int) (y: int) -> x * y";
    [%expect
      {|
      (ELet (NonRec, [("+", TUnit)],
         (EFun ([("x", TInt); ("y", TInt)], (EBinop (Mul, (EVar "x"), (EVar "y")))
            )),
         None)) |}]
  ;;
end

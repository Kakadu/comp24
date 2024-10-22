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
    [%expect {| (ELet (NonRec, "f", (EFun (("x", None), (EVar "x"))), None)) |}]
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
      (ELet (NonRec, "f",
         (EFun (("x", (Some TBool)),
            (EFun (("y", (Some TInt)), (EBinop (Mul, (EVar "x"), (EVar "y"))))))),
         None))
      |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "let (+) = fun (x: int) (y: int) -> x * y";
    [%expect
      {|
      (ELet (NonRec, "+",
         (EFun (("x", (Some TInt)),
            (EFun (("y", (Some TInt)), (EBinop (Mul, (EVar "x"), (EVar "y"))))))),
         None))
      |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "let (+) (x: int) y = let (-) x (y: int) = x + y in x - y";
    [%expect
      {|
      (ELet (NonRec, "+",
         (EFun (("x", (Some TInt)),
            (EFun (("y", None),
               (ELet (NonRec, "-",
                  (EFun (("x", None),
                     (EFun (("y", (Some TInt)),
                        (EBinop (Add, (EVar "x"), (EVar "y")))))
                     )),
                  (Some (EBinop (Sub, (EVar "x"), (EVar "y"))))))
               ))
            )),
         None)) 
      |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "let fix f = f (fix f) x in fix g";
    [%expect
      {|
      (ELet (NonRec, "fix",
         (EFun (("f", None),
            (EApp ((EApp ((EVar "f"), (EApp ((EVar "fix"), (EVar "f"))))),
               (EVar "x")))
            )),
         (Some (EApp ((EVar "fix"), (EVar "g"))))))
      |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "let f = fun (g: int -> int -> int) (x: int) (y: int) -> g x y";
    [%expect
      {|
      (ELet (NonRec, "f",
         (EFun (("g", (Some (TFun (TInt, (TFun (TInt, TInt)))))),
            (EFun (("x", (Some TInt)),
               (EFun (("y", (Some TInt)),
                  (EApp ((EApp ((EVar "g"), (EVar "x"))), (EVar "y")))))
               ))
            )),
         None))
      |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "let f (x: int * bool) = x";
    [%expect
      {|
      (ELet (NonRec, "f",
         (EFun (("x", (Some (TTuple (TInt, TBool, [])))), (EVar "x"))), None))
      |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "let f (x: (int * bool) -> unit) = x";
    [%expect
      {|
      (ELet (NonRec, "f",
         (EFun (("x", (Some (TFun ((TTuple (TInt, TBool, [])), TUnit)))),
            (EVar "x"))),
         None))
      |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "let f (x: int list) = x";
    [%expect
      {| (ELet (NonRec, "f", (EFun (("x", (Some (TList TInt))), (EVar "x"))), None)) |}]
  ;;

  let%expect_test _ =
    pp pp_expr parse_expr "let f (x: int * bool list * bool) = x";
    [%expect
      {|
      (ELet (NonRec, "f",
         (EFun (("x", (Some (TTuple (TInt, (TList TBool), [TBool])))), (EVar "x"))),
         None))
      |}]
  ;;
end

module TypecheckerTests = struct
  open Typechecker
  open Typing

  let pp_infer e =
    match run_infer e with
    | Ok ty -> Stdlib.Format.printf "%a" pp_ty ty
    | Error err -> Stdlib.Format.printf "%a" pp_error err
  ;;

  let pp_parse_and_infer input =
    match Parser.parse_expr input with
    | Result.Ok e -> pp_infer e
    | _ -> Stdlib.print_endline "Failed to parse"
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let x = (42, false, fun x -> x)";
    [%expect {| int * bool * ('a -> 'a) |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let f x y = [x; y; x = y]";
    [%expect {| bool -> bool -> bool list |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let f x y = [x; y] in f 42";
    [%expect {| int -> int list |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer
      "let rec fact x useless_var = if x = 1 then x else x * fact (x - 1) useless_var";
    [%expect {| int -> 'a -> int |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let rec fact x = if x = 1 then x else x * fact (x - 1) in fact 42";
    [%expect {| int |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let f x = match x with | (h :: tl1) :: tl2 -> true | _ -> false";
    [%expect {| 'a list list -> bool |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer
      {|
      let rec fold_left op acc xs = match xs with
      | []   -> acc
      | h :: t -> fold_left op (op acc h) t
      |};
    [%expect {| ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer
      {|
    let split xs = match xs with 
      | a :: b :: tl -> a, b, tl
    |};
    [%expect {| 'a list -> 'a * 'a * 'a list |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer
      {|
    let rec even_length xs = match xs with 
    | h :: h :: tl -> even_length tl
    | h :: [] -> false
    | _ -> true
    |};
    [%expect {| 'a list -> bool |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "[(fun x y -> x = y); (fun x y -> x <> y)]";
    [%expect {| ('a -> 'a -> bool) list |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "fun x -> x, fun x y -> x, y";
    [%expect {| 'a -> 'a * ('b -> 'c -> 'b * 'c) |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "(fun x -> x), (fun x y -> x, y)";
    [%expect {| ('a -> 'a) * ('b -> 'c -> 'b * 'c) |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer
      {| let permutation_of_123 l =
        match l with
        | 1 :: 2 :: 3 :: []
        | 1 :: 3 :: 2 :: []
        | 2 :: 1 :: 3 :: []
        | 2 :: 3 :: 1 :: []
        | 3 :: 1 :: 2 :: []
        | 3 :: 2 :: 1 :: [] -> true
        | _ -> false |};
    [%expect {| int list -> bool |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer
      "let f (x: int) (y: bool) (z: int) = if y then x, y, x + 1 else x + 1, y, x";
    [%expect {| int -> bool -> int -> int * bool * int |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let not_apply (g: int -> int) x = g";
    [%expect {| (int -> int) -> 'a -> int -> int |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer
      "let _ (f: (int -> int) -> unit) (g: int -> int -> int) (x: int) = f (g x)";
    [%expect {| ((int -> int) -> unit) -> (int -> int -> int) -> int -> unit |}]
  ;;

  (* Errors *)

  let%expect_test _ =
    pp_parse_and_infer "let f x = x + y";
    [%expect {| Unbound value y |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let rec f x = f";
    [%expect {| The type variable 'a occurs inside 'b -> 'a |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "[1; 1, 2]";
    [%expect {| Failed to unify types int and int * int |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let f x = [fun x -> x + x; fun x -> x >= x]";
    [%expect {| Failed to unify types int and bool |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let () = if true then 1";
    [%expect {| Failed to unify types int and unit |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer
      "let f x = match x with | a :: b -> a | ((a :: true) :: c) :: tl -> c ";
    [%expect {| Failed to unify types 'a list and bool |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer
      "let f x = match x with | a :: b :: y :: tl | a :: b :: z :: tl -> tl";
    [%expect {| Variable z doesn't occure in some 'or' patterns |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let f x = match x with | a :: 1 :: _ | a :: false :: _ -> a";
    [%expect
      {| Variable a has different types in 'or' patterns: int and bool are not equal |}]
  ;;

  let%expect_test _ =
    pp_parse_and_infer "let apply (g: int -> int) (x: bool) = g x";
    [%expect {| Failed to unify types int and bool |}]
  ;;
end

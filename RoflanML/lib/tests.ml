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

  let%expect_test "simple function definition" =
    pp pp_expr parse_expr "let f x = x";
    [%expect {| (ELet (NonRec, "f", (EFun (("x", None), (EVar "x"))), None)) |}]
  ;;

  let%expect_test "if-then-else branch" =
    pp pp_expr parse_expr "if f x then x else 0";
    [%expect
      {| (EBranch ((EApp ((EVar "f"), (EVar "x"))), (EVar "x"), (EConst (CInt 0)))) |}]
  ;;

  let%expect_test "list with arithmetic operations" =
    pp pp_expr parse_expr "[1 + 2; 1 * 2; 1 - 2; 1 / 2]";
    [%expect
      {|
      (EList
         [(EApp ((EApp ((EVar "+"), (EConst (CInt 1)))), (EConst (CInt 2))));
           (EApp ((EApp ((EVar "*"), (EConst (CInt 1)))), (EConst (CInt 2))));
           (EApp ((EApp ((EVar "-"), (EConst (CInt 1)))), (EConst (CInt 2))));
           (EApp ((EApp ((EVar "/"), (EConst (CInt 1)))), (EConst (CInt 2))))])
      |}]
  ;;

  let%expect_test "match expression with pattern matching" =
    pp
      pp_expr
      parse_expr
      {|match x with | h1 :: h2 :: tl -> if h1 >= h2 then h1 else h2 | h1 :: [] -> h1 | _ -> 0|};
    [%expect
      {|
      (EMatch ((EVar "x"),
         [((PCons ((PVar "h1"), (PVar "h2"), [(PVar "tl")])),
           (EBranch ((EApp ((EApp ((EVar ">="), (EVar "h1"))), (EVar "h2"))),
              (EVar "h1"), (EVar "h2"))));
           ((PCons ((PVar "h1"), PEmpty, [])), (EVar "h1"));
           ((PVar "_"), (EConst (CInt 0)))]
         ))
      |}]
  ;;

  let%expect_test "tuple of lists" =
    pp pp_expr parse_expr "[1; 2], [3; 4]";
    [%expect
      {|
    (ETuple ((EList [(EConst (CInt 1)); (EConst (CInt 2))]),
       (EList [(EConst (CInt 3)); (EConst (CInt 4))]), [])) 
    |}]
  ;;

  let%expect_test "nested tuples in a list" =
    pp pp_expr parse_expr "[1, 2, 3; 4, 5, 6]";
    [%expect
      {|
    (EList
       [(ETuple ((EConst (CInt 1)), (EConst (CInt 2)), [(EConst (CInt 3))]));
         (ETuple ((EConst (CInt 4)), (EConst (CInt 5)), [(EConst (CInt 6))]))]) 
    |}]
  ;;

  let%expect_test "unit let binding" =
    pp pp_expr parse_expr "let () = () in ()";
    [%expect {| (ELet (NonRec, "()", (EConst CUnit), (Some (EConst CUnit)))) |}]
  ;;

  let%expect_test "function with type annotation" =
    pp pp_expr parse_expr "let f (x : bool) (y: int)= x * y";
    [%expect
      {|
      (ELet (NonRec, "f",
         (EFun (("x", (Some TBool)),
            (EFun (("y", (Some TInt)),
               (EApp ((EApp ((EVar "*"), (EVar "x"))), (EVar "y")))))
            )),
         None))
      |}]
  ;;

  let%expect_test "operator function definition" =
    pp pp_expr parse_expr "let (+) = fun (x: int) (y: int) -> x * y";
    [%expect
      {|
      (ELet (NonRec, "+",
         (EFun (("x", (Some TInt)),
            (EFun (("y", (Some TInt)),
               (EApp ((EApp ((EVar "*"), (EVar "x"))), (EVar "y")))))
            )),
         None))
      |}]
  ;;

  let%expect_test "nested let and operations in function" =
    pp pp_expr parse_expr "let ( + ) (x: int) y = let ( - ) x (y: int) = x + y in x - y";
    [%expect
      {|
      (ELet (NonRec, "+",
         (EFun (("x", (Some TInt)),
            (EFun (("y", None),
               (ELet (NonRec, "-",
                  (EFun (("x", None),
                     (EFun (("y", (Some TInt)),
                        (EApp ((EApp ((EVar "+"), (EVar "x"))), (EVar "y")))))
                     )),
                  (Some (EApp ((EApp ((EVar "-"), (EVar "x"))), (EVar "y"))))))
               ))
            )),
         None)) 
      |}]
  ;;

  let%expect_test "recursive function with nested application" =
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

  let%expect_test "higher order function with tuple and list" =
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

  let%expect_test "tuple argument type in function" =
    pp pp_expr parse_expr "let f (x: int * bool) = x";
    [%expect
      {|
      (ELet (NonRec, "f",
         (EFun (("x", (Some (TTuple (TInt, TBool, [])))), (EVar "x"))), None))
      |}]
  ;;

  let%expect_test "function with tuple argument to unit type" =
    pp pp_expr parse_expr "let f (x: (int * bool) -> unit) = x";
    [%expect
      {|
      (ELet (NonRec, "f",
         (EFun (("x", (Some (TFun ((TTuple (TInt, TBool, [])), TUnit)))),
            (EVar "x"))),
         None))
      |}]
  ;;

  let%expect_test "function with list type argument" =
    pp pp_expr parse_expr "let f (x: int list) = x";
    [%expect
      {| (ELet (NonRec, "f", (EFun (("x", (Some (TList TInt))), (EVar "x"))), None)) |}]
  ;;

  let%expect_test "function with complex tuple and list types" =
    pp pp_expr parse_expr "let f (x: int * bool list * bool) = x";
    [%expect
      {|
      (ELet (NonRec, "f",
         (EFun (("x", (Some (TTuple (TInt, (TList TBool), [TBool])))), (EVar "x"))),
         None))
      |}]
  ;;

  let%expect_test "application of operator within function body" =
    pp pp_expr parse_expr "let f x y = ( > ) x y";
    [%expect
      {|
      (ELet (NonRec, "f",
         (EFun (("x", None),
            (EFun (("y", None),
               (EApp ((EApp ((EVar ">"), (EVar "x"))), (EVar "y")))))
            )),
         None))
      |}]
  ;;

  let%expect_test "nested let with function and operator application" =
    pp pp_expr parse_expr "let x = let f g x y = g x y in f ( + ) 7 8";
    [%expect
      {|
      (ELet (NonRec, "x",
         (ELet (NonRec, "f",
            (EFun (("g", None),
               (EFun (("x", None),
                  (EFun (("y", None),
                     (EApp ((EApp ((EVar "g"), (EVar "x"))), (EVar "y")))))
                  ))
               )),
            (Some (EApp (
                     (EApp ((EApp ((EVar "f"), (EVar "+"))), (EConst (CInt 7)))),
                     (EConst (CInt 8)))))
            )),
         None))
      |}]
  ;;

  let%expect_test "chained equality in custom operator function" =
    pp pp_expr parse_expr "let ( ^-^ ) x y z = x = y = z";
    [%expect
      {|
      (ELet (NonRec, "^-^",
         (EFun (("x", None),
            (EFun (("y", None),
               (EFun (("z", None),
                  (EApp (
                     (EApp ((EVar "="),
                        (EApp ((EApp ((EVar "="), (EVar "x"))), (EVar "y"))))),
                     (EVar "z")))
                  ))
               ))
            )),
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

  let%expect_test "type inference for tuple with function" =
    pp_parse_and_infer "let x = (42, false, fun x -> x)";
    [%expect {| int * bool * ('a -> 'a) |}]
  ;;

  let%expect_test "inference for list of booleans with equality" =
    pp_parse_and_infer "let f x y = [x; y; x = y]";
    [%expect {| bool -> bool -> bool list |}]
  ;;

  let%expect_test "inference for function returning a list" =
    pp_parse_and_infer "let f x y = [x; y] in f 42";
    [%expect {| int -> int list |}]
  ;;

  let%expect_test "inference for recursive factorial function" =
    pp_parse_and_infer
      "let rec fact x useless_var = if x = 1 then x else x * fact (x - 1) useless_var";
    [%expect {| int -> 'a -> int |}]
  ;;

  let%expect_test "type of recursive factorial function application" =
    pp_parse_and_infer "let rec fact x = if x = 1 then x else x * fact (x - 1) in fact 42";
    [%expect {| int |}]
  ;;

  let%expect_test "nested list matching in function" =
    pp_parse_and_infer "let f x = match x with | (h :: tl1) :: tl2 -> true | _ -> false";
    [%expect {| 'a list list -> bool |}]
  ;;

  let%expect_test "type inference for fold_left implementation" =
    pp_parse_and_infer
      {|
      let rec fold_left op acc xs = match xs with
      | []   -> acc
      | h :: t -> fold_left op (op acc h) t
      |};
    [%expect {| ('b -> 'a -> 'b) -> 'b -> 'a list -> 'b |}]
  ;;

  let%expect_test "inference for pattern matching in split function" =
    pp_parse_and_infer
      {|
    let split xs = match xs with 
      | a :: b :: tl -> a, b, tl
    |};
    [%expect {| 'a list -> 'a * 'a * 'a list |}]
  ;;

  let%expect_test "even length function with list patterns" =
    pp_parse_and_infer
      {|
    let rec even_length xs = match xs with 
    | h :: h :: tl -> even_length tl
    | h :: [] -> false
    | _ -> true
    |};
    [%expect {| 'a list -> bool |}]
  ;;

  let%expect_test "list of comparison functions" =
    pp_parse_and_infer "[(fun x y -> x = y); (fun x y -> x <> y)]";
    [%expect {| ('a -> 'a -> bool) list |}]
  ;;

  let%expect_test "inference for nested functions in tuple" =
    pp_parse_and_infer "fun x -> x, fun x y -> x, y";
    [%expect {| 'a -> 'a * ('b -> 'c -> 'b * 'c) |}]
  ;;

  let%expect_test "inference for nested functions in tuple" =
    pp_parse_and_infer "(fun x -> x), (fun x y -> x, y)";
    [%expect {| ('a -> 'a) * ('b -> 'c -> 'b * 'c) |}]
  ;;

  let%expect_test "permutation check on list patterns" =
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

  let%expect_test "inference for conditional with mixed return types" =
    pp_parse_and_infer
      "let f (x: int) (y: bool) (z: int) = if y then x, y, x + 1 else x + 1, y, x";
    [%expect {| int -> bool -> int -> int * bool * int |}]
  ;;

  let%expect_test "partial application of function with annotation" =
    pp_parse_and_infer "let not_apply (g: int -> int) x = g";
    [%expect {| (int -> int) -> 'a -> int -> int |}]
  ;;

  let%expect_test "function passing as argument with annotation" =
    pp_parse_and_infer
      "let _ (f: (int -> int) -> unit) (g: int -> int -> int) (x: int) = f (g x)";
    [%expect {| ((int -> int) -> unit) -> (int -> int -> int) -> int -> unit |}]
  ;;

  let%expect_test "inference for custom operator with multiple arguments" =
    pp_parse_and_infer "let ( + ) x y z = x * y * z in 1 + 2";
    [%expect {| int -> int |}]
  ;;

  let%expect_test "chained comparison in custom operator" =
    pp_parse_and_infer "let ( ^-^ ) x y z = x = y = z";
    [%expect {| 'a -> 'a -> bool -> bool |}]
  ;;

  (* Errors *)

  let%expect_test "unbound value error in expression" =
    pp_parse_and_infer "let f x = x + y";
    [%expect {| Unbound value y |}]
  ;;

  let%expect_test "type recursion error in self-reference" =
    pp_parse_and_infer "let rec f x = f";
    [%expect {| The type variable 'a occurs inside 'b -> 'a |}]
  ;;

  let%expect_test "type mismatch error in list of mixed types" =
    pp_parse_and_infer "[1; 1, 2]";
    [%expect {| Failed to unify types int and int * int |}]
  ;;

  let%expect_test "type mismatch in function list with different return types" =
    pp_parse_and_infer "let f x = [fun x -> x + x; fun x -> x >= x]";
    [%expect {| Failed to unify types int and bool |}]
  ;;

  let%expect_test "type mismatch in conditional branches" =
    pp_parse_and_infer "let () = if true then 1";
    [%expect {| Failed to unify types int and unit |}]
  ;;

  let%expect_test "pattern matching type error in nested lists" =
    pp_parse_and_infer
      "let f x = match x with | a :: b -> a | ((a :: true) :: c) :: tl -> c ";
    [%expect {| Failed to unify types 'a list and bool |}]
  ;;

  let%expect_test "error in 'or' patterns with unmatched variable" =
    pp_parse_and_infer
      "let f x = match x with | a :: b :: y :: tl | a :: b :: z :: tl -> tl";
    [%expect {| Variable z doesn't occure in some 'or' patterns |}]
  ;;

  let%expect_test "type error in 'or' pattern with conflicting types" =
    pp_parse_and_infer "let f x = match x with | a :: 1 :: _ | a :: false :: _ -> a";
    [%expect
      {| Variable a has different types in 'or' patterns: int and bool are not equal |}]
  ;;

  let%expect_test "argument type mismatch in function application" =
    pp_parse_and_infer "let apply (g: int -> int) (x: bool) = g x";
    [%expect {| Failed to unify types int and bool |}]
  ;;
end

module UnparseTests = struct
  open Unparse

  let%expect_test "print constant" =
    let ast = [ EConst (CInt 42) ] in
    let printed = unparse_program ast in
    Format.printf "%s\n" printed;
    [%expect {| 42 |}]
  ;;

  let%expect_test "print variable" =
    let ast = [ EVar "x" ] in
    let printed = unparse_program ast in
    Format.printf "%s\n" printed;
    [%expect {| x |}]
  ;;

  let%expect_test "print let-expression" =
    (* let x = 1 in x *)
    let ast = [ ELet (NonRec, "x", EConst (CInt 1), Some (EVar "x")) ] in
    let printed = unparse_program ast in
    Format.printf "%s\n" printed;
    [%expect {| let x = 1 in x |}]
  ;;

  let%expect_test "print function with if-then-else" =
    let arg = "x", Some TInt in
    let body = EBranch (EVar "x", EConst (CInt 1), EConst (CInt 0)) in
    let ast = [ EFun (arg, body) ] in
    let printed = unparse_program ast in
    Format.printf "%s\n" printed;
    [%expect {| fun (x : int) -> if x then 1 else 0 |}]
  ;;
end

module QCheckTests = struct
  open Unparse

  let arbitrary_decl =
    QCheck.make
      (QCheck.Gen.sized (fun n ->
         QCheck.Gen.map
           (fun e -> [ ELet (NonRec, "x", e, None) ])
           (Check.Generator.gen_expr (min n 500))))
      ~print:unparse_program
      ~shrink:Check.Shrinker.shrink_program
  ;;

  let parser_qtests =
    [ QCheck.Test.make ~count:100 arbitrary_decl (fun ast ->
        let src = unparse_program ast in
        match Parser.parse src with
        | Result.Ok ast' when ast' = ast -> true
        | Result.Ok ast' ->
          Format.printf
            "\n\n[!]: Different AST!\nSource: %S\nParsed: %s\nOriginal: %s\n"
            src
            (Ast.show_program ast')
            (Ast.show_program ast);
          false
        | Result.Error err ->
          Format.printf "\n\n[!]: Parser error: %s\nOn source:\n%S\n" err src;
          false)
    ]
  ;;

  let%expect_test "QuickCheck round-trip test for declarations (depth ≤ 3, no shrinker)" =
    QCheck_runner.set_seed 42;
    let _ = QCheck_runner.run_tests ~colors:false parser_qtests in
    ();
    [%expect
      {|
      random seed: 42
      ================================================================================
      success (ran 1 tests)
      |}]
  ;;
end

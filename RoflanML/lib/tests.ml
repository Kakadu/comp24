(* Copyright 2024, Ilya Syresenkov, Akhmetov Tamerlan *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Roflanml_lib
open Ast
open Ast_to_str
open Parser

module ParserTests = struct
  let parse_and_pp_expr input =
    match parse_expr input with
    | Result.Ok res -> Stdlib.Format.printf "%a" pp_expr res
    | _ -> Stdlib.print_endline "Failed to parse"
  ;;

  let parse_and_pp_decl input =
    match parse_decl input with
    | Result.Ok res -> Stdlib.Format.printf "%a" pp_decl res
    | _ -> Stdlib.print_endline "Failed to parse"
  ;;

  let%expect_test "simple function definition" =
    parse_and_pp_decl "let f x = x";
    [%expect {| (DLet (NonRec, "f", (EFun (("x", None), (EVar "x"))))) |}]
  ;;

  let%expect_test "if-then-else branch" =
    parse_and_pp_expr "if f x then x else 0";
    [%expect
      {| (EBranch ((EApp ((EVar "f"), (EVar "x"))), (EVar "x"), (EConst (CInt 0)))) |}]
  ;;

  let%expect_test "list with arithmetic operations" =
    parse_and_pp_expr "[1 + 2; 1 * 2; 1 - 2; 1 / 2]";
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
    parse_and_pp_expr
      {|match x with | h1 :: h2 :: tl -> if h1 >= h2 then h1 else h2 | h1 :: [] -> h1 | _ -> 0|};
    [%expect
      {|
      (EMatch ((EVar "x"),
         [((PCons ((PVar "h1"), (PVar "h2"), [(PVar "tl")])),
           (EBranch ((EApp ((EApp ((EVar ">="), (EVar "h1"))), (EVar "h2"))),
              (EVar "h1"), (EVar "h2"))));
           ((PCons ((PVar "h1"), PEmpty, [])), (EVar "h1"));
           (PWild, (EConst (CInt 0)))]
         ))
      |}]
  ;;

  let%expect_test "tuple of lists" =
    parse_and_pp_expr "[1; 2], [3; 4]";
    [%expect
      {|
    (ETuple ((EList [(EConst (CInt 1)); (EConst (CInt 2))]),
       (EList [(EConst (CInt 3)); (EConst (CInt 4))]), [])) 
    |}]
  ;;

  let%expect_test "nested tuples in a list" =
    parse_and_pp_expr "[1, 2, 3; 4, 5, 6]";
    [%expect
      {|
    (EList
       [(ETuple ((EConst (CInt 1)), (EConst (CInt 2)), [(EConst (CInt 3))]));
         (ETuple ((EConst (CInt 4)), (EConst (CInt 5)), [(EConst (CInt 6))]))]) 
    |}]
  ;;

  let%expect_test "unit let binding" =
    parse_and_pp_expr "let () = () in ()";
    [%expect {| (ELetIn (NonRec, "()", (EConst CUnit), (EConst CUnit))) |}]
  ;;

  let%expect_test "function with type annotation" =
    parse_and_pp_decl "let f (x : bool) (y: int)= x * y";
    [%expect
      {|
      (DLet (NonRec, "f",
         (EFun (("x", (Some TBool)),
            (EFun (("y", (Some TInt)),
               (EApp ((EApp ((EVar "*"), (EVar "x"))), (EVar "y")))))
            ))
         ))
      |}]
  ;;

  let%expect_test "operator function definition" =
    parse_and_pp_decl "let (+) = fun (x: int) (y: int) -> x * y";
    [%expect
      {|
      (DLet (NonRec, "+",
         (EFun (("x", (Some TInt)),
            (EFun (("y", (Some TInt)),
               (EApp ((EApp ((EVar "*"), (EVar "x"))), (EVar "y")))))
            ))
         ))
      |}]
  ;;

  let%expect_test "nested let and operations in function" =
    parse_and_pp_decl "let ( + ) (x: int) y = let ( - ) x (y: int) = x + y in x - y";
    [%expect
      {|
      (DLet (NonRec, "+",
         (EFun (("x", (Some TInt)),
            (EFun (("y", None),
               (ELetIn (NonRec, "-",
                  (EFun (("x", None),
                     (EFun (("y", (Some TInt)),
                        (EApp ((EApp ((EVar "+"), (EVar "x"))), (EVar "y")))))
                     )),
                  (EApp ((EApp ((EVar "-"), (EVar "x"))), (EVar "y")))))
               ))
            ))
         ))
      |}]
  ;;

  let%expect_test "recursive function with nested application" =
    parse_and_pp_expr "let fix f = f (fix f) x in fix g";
    [%expect
      {|
      (ELetIn (NonRec, "fix",
         (EFun (("f", None),
            (EApp ((EApp ((EVar "f"), (EApp ((EVar "fix"), (EVar "f"))))),
               (EVar "x")))
            )),
         (EApp ((EVar "fix"), (EVar "g")))))
      |}]
  ;;

  let%expect_test "higher order function with tuple and list" =
    parse_and_pp_decl "let f = fun (g: int -> int -> int) (x: int) (y: int) -> g x y";
    [%expect
      {|
      (DLet (NonRec, "f",
         (EFun (("g", (Some (TFun (TInt, (TFun (TInt, TInt)))))),
            (EFun (("x", (Some TInt)),
               (EFun (("y", (Some TInt)),
                  (EApp ((EApp ((EVar "g"), (EVar "x"))), (EVar "y")))))
               ))
            ))
         ))
      |}]
  ;;

  let%expect_test "tuple argument type in function" =
    parse_and_pp_decl "let f (x: int * bool) = x";
    [%expect
      {|
      (DLet (NonRec, "f",
         (EFun (("x", (Some (TTuple (TInt, TBool, [])))), (EVar "x")))))
      |}]
  ;;

  let%expect_test "function with tuple argument to unit type" =
    parse_and_pp_decl "let f (x: (int * bool) -> unit) = x";
    [%expect
      {|
      (DLet (NonRec, "f",
         (EFun (("x", (Some (TFun ((TTuple (TInt, TBool, [])), TUnit)))),
            (EVar "x")))
         ))
      |}]
  ;;

  let%expect_test "function with list type argument" =
    parse_and_pp_decl "let f (x: int list) = x";
    [%expect {| (DLet (NonRec, "f", (EFun (("x", (Some (TList TInt))), (EVar "x"))))) |}]
  ;;

  let%expect_test "function with complex tuple and list types" =
    parse_and_pp_decl "let f (x: int * bool list * bool) = x";
    [%expect
      {|
      (DLet (NonRec, "f",
         (EFun (("x", (Some (TTuple (TInt, (TList TBool), [TBool])))), (EVar "x")))
         ))
      |}]
  ;;

  let%expect_test "application of operator within function body" =
    parse_and_pp_decl "let f x y = ( > ) x y";
    [%expect
      {|
      (DLet (NonRec, "f",
         (EFun (("x", None),
            (EFun (("y", None),
               (EApp ((EApp ((EVar ">"), (EVar "x"))), (EVar "y")))))
            ))
         ))
      |}]
  ;;

  let%expect_test "nested let with function and operator application" =
    parse_and_pp_decl "let x = let f g x y = g x y in f ( + ) 7 8";
    [%expect
      {|
      (DLet (NonRec, "x",
         (ELetIn (NonRec, "f",
            (EFun (("g", None),
               (EFun (("x", None),
                  (EFun (("y", None),
                     (EApp ((EApp ((EVar "g"), (EVar "x"))), (EVar "y")))))
                  ))
               )),
            (EApp ((EApp ((EApp ((EVar "f"), (EVar "+"))), (EConst (CInt 7)))),
               (EConst (CInt 8))))
            ))
         ))
      |}]
  ;;

  let%expect_test "chained equality in custom operator function" =
    parse_and_pp_decl "let ( ^-^ ) x y z = x = y = z";
    [%expect
      {|
      (DLet (NonRec, "^-^",
         (EFun (("x", None),
            (EFun (("y", None),
               (EFun (("z", None),
                  (EApp (
                     (EApp ((EVar "="),
                        (EApp ((EApp ((EVar "="), (EVar "x"))), (EVar "y"))))),
                     (EVar "z")))
                  ))
               ))
            ))
         ))
      |}]
  ;;

  let%expect_test "tuple pattern" =
    parse_and_pp_decl
      "let ( =| ) x = match x with\n    |(a, b, ilyaChert) -> 1\n    | (x, true) -> 2";
    [%expect
      {|
      (DLet (NonRec, "=|",
         (EFun (("x", None),
            (EMatch ((EVar "x"),
               [((PTuple ((PVar "a"), (PVar "b"), [(PVar "ilyaChert")])),
                 (EConst (CInt 1)));
                 ((PTuple ((PVar "x"), (PConst (CBool true)), [])),
                  (EConst (CInt 2)))
                 ]
               ))
            ))
         ))
      |}]
  ;;
end

module TypecheckerTests = struct
  open Typechecker
  open Typing

  let pp_infer decl =
    match run_infer decl with
    | Ok tys -> List.iter tys ~f:(Stdlib.Format.printf "%a\n" pp_ty)
    | Error err -> Stdlib.Format.printf "%a" pp_error err
  ;;

  let pp_parse_and_infer input =
    match Parser.parse_decl input with
    | Result.Ok decl -> pp_infer decl
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
    pp_parse_and_infer "let q = let f x y = [x; y] in f 42";
    [%expect {| int -> int list |}]
  ;;

  let%expect_test "inference for recursive factorial function" =
    pp_parse_and_infer
      "let rec fact x useless_var = if x = 1 then x else x * fact (x - 1) useless_var";
    [%expect {| int -> 'a -> int |}]
  ;;

  let%expect_test "type of recursive factorial function application" =
    pp_parse_and_infer
      "let x = let rec fact x = if x = 1 then x else x * fact (x - 1) in fact 42";
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
    pp_parse_and_infer "let l = [(fun x y -> x = y); (fun x y -> x <> y)]";
    [%expect {| ('a -> 'a -> bool) list |}]
  ;;

  let%expect_test "inference for nested functions in tuple" =
    pp_parse_and_infer "let fs = fun x -> x, fun x y -> x, y";
    [%expect {| 'a -> 'a * ('b -> 'c -> 'b * 'c) |}]
  ;;

  let%expect_test "inference for nested functions in tuple" =
    pp_parse_and_infer "let fs = (fun x -> x), (fun x y -> x, y)";
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
    pp_parse_and_infer "let q = let ( + ) x y z = x * y * z in 1 + 2";
    [%expect {| int -> int |}]
  ;;

  let%expect_test "chained comparison in custom operator" =
    pp_parse_and_infer "let ( ^-^ ) x y z = x = y = z";
    [%expect {| 'a -> 'a -> bool -> bool |}]
  ;;

  let%expect_test "stdlib print_int" =
    pp_parse_and_infer "let print = print_int";
    [%expect {| int -> unit |}]
  ;;

  let%expect_test "stdlib print_bool" =
    pp_parse_and_infer "let print = print_bool";
    [%expect {| bool -> unit |}]
  ;;

  let%expect_test "tuple pattern poly type inference" =
    pp_parse_and_infer "let f x = match x with | (x, y) -> y, x";
    [%expect {| 'a * 'b -> 'b * 'a |}]
  ;;

  let%expect_test "mutual recursion type inference 1" =
    pp_parse_and_infer
      "let rec even x = if x = 0 then true else odd (x - 1) and odd x = if x = 0 then \
       false else even (x - 1)";
    [%expect
      {|
      int -> bool
      int -> bool
      |}]
  ;;

  let%expect_test "mutual recursion type inference 2" =
    pp_parse_and_infer "let rec f x = x && true and g x = x + 1";
    [%expect
      {|
      bool -> bool
      int -> int
      |}]
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
    pp_parse_and_infer "let l = [1; 1, 2]";
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

  let%expect_test "mutual recursion occurs check" =
    pp_parse_and_infer "let rec f x = g and g x = f";
    [%expect {| The type variable 'a occurs inside 'c -> 'b -> 'a |}]
  ;;
end

module CCTests = struct
  open Closure_conversion
  open Roflanml_stdlib

  let env = RoflanML_Stdlib.default |> Map.keys |> Set.of_list (module String)

  let pp_parse_and_cc input =
    match Parser.parse_decl input with
    | Result.Ok e -> Stdlib.Format.printf "%s" (ast_to_str (close e env))
    | _ -> Stdlib.print_endline "Failed to parse"
  ;;

  let%expect_test "no closure" =
    pp_parse_and_cc "let q = let f x = x in f 10";
    [%expect
      {|
      let q =
      let f x = x in f 10
      |}]
  ;;

  let%expect_test "let closure" =
    pp_parse_and_cc "let q = let f x = x + 1 in let g x = f x in g 10";
    [%expect
      {|
      let q =
      let f x = ( + ) x 1 in
      let g f x = f x in g f 10
      |}]
  ;;

  let%expect_test "let chain closure" =
    pp_parse_and_cc
      "let q = let f x = x + 1 in let g x = f x in let k x = g x in let q x = k x in q 1";
    [%expect
      {|
      let q =
      let f x = ( + ) x 1 in
      let g f x = f x in
      let k f g x = g f x in
      let q f g k x = k f g x in q f g k 1
      |}]
  ;;

  let%expect_test "fun closure" =
    pp_parse_and_cc "let q = let x = 1 in let y = 2 in fun z -> x + y + z";
    [%expect
      {|
      let q =
      let x = 1 in
      let y = 2 in (fun x y z -> ( + ) (( + ) x y) z) x y
      |}]
  ;;

  let%expect_test "fun chain closure" =
    pp_parse_and_cc "let q = let x = 1 in (fun f y -> (f x) + x + 1) (fun z -> x + z)";
    [%expect
      {|
      let q =
      let x = 1 in (fun x f y -> ( + ) (( + ) (f x) x) 1) x ((fun x z -> ( + ) x z) x)
      |}]
  ;;

  let%expect_test "no closure with stdlib" =
    pp_parse_and_cc "let f x = print_int x";
    [%expect {| let f x = print_int x |}]
  ;;

  let%expect_test "closure with rec" =
    pp_parse_and_cc "let q = let f x = x + 1 in let rec g x = f x + g x in g";
    [%expect
      {|
      let q =
      let f x = ( + ) x 1 in
      let rec g f x = ( + ) (f x) (g f x) in g f
      |}]
  ;;

  let%expect_test "cps fact" =
    pp_parse_and_cc
      "let q = let rec fact x k = match x with | 1 -> k 1 | x -> fact (x - 1) (fun n -> \
       k (x * n)) in fact 10 (fun x -> x)";
    [%expect
      {|
      let q =
      let rec fact x k = match x with
      | 1 -> k 1
      | x -> fact (( - ) x 1) ((fun k x n -> k (( * ) x n)) k x) in fact 10 (fun x -> x)
      |}]
  ;;

  let%expect_test "mutual recursion" =
    pp_parse_and_cc
      "let rec even x = if x = 0 then true else odd (x - 1) and odd x = if x = 0 then \
       false else even (x - 1)";
    [%expect
      {|
      let rec even = (fun odd x -> if ( = ) x 0 then true else odd (( - ) x 1))
      and
      odd = (fun even x -> if ( = ) x 0 then false else even (( - ) x 1))
      |}]
  ;;
end

module LLTests = struct
  open Closure_conversion
  open Lambda_lifting
  open Roflanml_stdlib
  open Ll_ast

  let env = RoflanML_Stdlib.default |> Map.keys |> Set.of_list (module String)

  let pp_parse_and_ll input =
    match Parser.parse input with
    | Result.Ok prog ->
      (match lift_program (close_program prog env) with
       | Result.Ok prog ->
         List.iter prog ~f:(fun decl ->
           Stdlib.Format.printf "%s\n" (ast_to_str (ll_to_ast decl)))
       | Result.Error _ -> Stdlib.print_endline "Failed to LL")
    | _ -> Stdlib.print_endline "Failed to parse"
  ;;

  let%expect_test _ =
    pp_parse_and_ll
      "let q = let rec fact x k = match x with | 1 -> k 1 | x -> fact (x - 1) (fun n -> \
       k (x * n)) in fact 10 (fun x -> x)";
    [%expect
      {|
      let LL_1 k x n = k (( * ) x n)

      let rec LL_0 x k = match x with
      | 1 -> k 1
      | x -> LL_0 (( - ) x 1) (LL_1 k x)

      let LL_2 x = x

      let q = LL_0 10 LL_2
      |}]
  ;;
end

module ANFTests = struct
  open Closure_conversion
  open Lambda_lifting
  open Roflanml_stdlib
  open Anf_ast
  open Anf

  let env = RoflanML_Stdlib.default |> Map.keys |> Set.of_list (module String)

  let pp_parse_and_anf input =
    match Parser.parse input with
    | Result.Ok prog ->
      (match lift_program (close_program prog env) with
       | Result.Ok prog ->
         (match anf_program prog with
          | Result.Ok prog ->
            List.iter prog ~f:(fun decl ->
              Stdlib.Format.printf "%s\n" (ast_to_str (anf_to_ast decl)))
          | Result.Error err -> Stdlib.print_endline err)
       | Result.Error _ -> Stdlib.print_endline "Failed to LL")
    | _ -> Stdlib.print_endline "Failed to parse"
  ;;

  let%expect_test "anf simple" =
    pp_parse_and_anf "let q = 1 + 2 + 3 * 4 + 5";
    [%expect
      {|
      let q =
      let ANF_0 = ( + ) 1 2 in
      let ANF_1 = ( * ) 3 4 in
      let ANF_2 = ( + ) ANF_0 ANF_1 in
      let ANF_3 = ( + ) ANF_2 5 in ANF_3
      |}]
  ;;

  let%expect_test "anf tuple" =
    pp_parse_and_anf "let q = let f x y z = x + y + z in (f 1 2 3, 4, 5)";
    [%expect
      {|
      let LL_0 x y z =
      let ANF_2 = ( + ) x y in
      let ANF_3 = ( + ) ANF_2 z in ANF_3

      let q =
      let ANF_0 = LL_0 1 2 3 in
      let ANF_1 = ANF_0, 4, 5 in ANF_1
      |}]
  ;;

  let%expect_test "anf partian app" =
    pp_parse_and_anf "let q = let f x y z = x + y + z in let g x = f 1 2 in g 3";
    [%expect
      {|
      let LL_0 x y z =
      let ANF_2 = ( + ) x y in
      let ANF_3 = ( + ) ANF_2 z in ANF_3

      let LL_1 f x =
      let ANF_1 = LL_0 1 2 in ANF_1

      let q =
      let ANF_0 = LL_1 LL_0 3 in ANF_0
      |}]
  ;;

  let%expect_test "anf branch" =
    pp_parse_and_anf
      "let q = if true && false then let f x = x + 1 in f 1 else let g x = x - 1 in g 1";
    [%expect
      {|
      let LL_0 x =
      let ANF_5 = ( + ) x 1 in ANF_5

      let LL_1 x =
      let ANF_4 = ( - ) x 1 in ANF_4

      let q =
      let ANF_0 = ( && ) true false in
      let ANF_1 = if ANF_0 then
      let ANF_2 = LL_0 1 in ANF_2 else
      let ANF_3 = LL_1 1 in ANF_3 in ANF_1
      |}]
  ;;

  let%expect_test "anf closure" =
    pp_parse_and_anf "let q = let x = 1 in let y = 2 in x / y";
    [%expect
      {|
      let q =
      let x = 1 in
      let y = 2 in
      let ANF_0 = ( / ) x y in ANF_0
      |}]
  ;;

  let%expect_test "anf lists" =
    pp_parse_and_anf "let q = [(fun x -> x * x) 1; (fun x -> x / x) 2]";
    [%expect
      {|
      let LL_0 x =
      let ANF_4 = ( * ) x x in ANF_4

      let LL_1 x =
      let ANF_3 = ( / ) x x in ANF_3

      let q =
      let ANF_0 = LL_0 1 in
      let ANF_1 = LL_1 2 in
      let ANF_2 = [ ANF_0; ANF_1 ] in ANF_2
      |}]
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

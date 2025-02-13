(** Copyright 2025, tepa46 *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML

let print_result val_pp = function
  | Ok v -> Stdlib.Format.printf "%a" val_pp v
  | Error _ -> Stdlib.Printf.printf "Syntax error"
;;

(***************************Identifier*Parser*Tests***************************)

let parse_and_print s = Parser.id_from_string s |> print_result Ast.pp_identifier

let%expect_test "" =
  parse_and_print {| variable |};
  [%expect {| (Id "variable") |}]
;;

let%expect_test "" =
  parse_and_print {| hEaD_52_tAiL_ |};
  [%expect {| (Id "hEaD_52_tAiL_") |}]
;;

let%expect_test "" =
  parse_and_print {| (+) |};
  [%expect {| (Id "( + )") |}]
;;

let%expect_test "" =
  parse_and_print {| ( + ) |};
  [%expect {| (Id "( + )") |}]
;;

let%expect_test "" =
  parse_and_print {| ( +) |};
  [%expect {| (Id "( + )") |}]
;;

let%expect_test "" =
  parse_and_print {| + |};
  [%expect {| Syntax error |}]
;;

let%expect_test "" =
  parse_and_print {| ( +* ) |};
  [%expect {| (Id "( +* )") |}]
;;

let%expect_test "" =
  parse_and_print {| +* |};
  [%expect {| Syntax error |}]
;;

let%expect_test "" =
  parse_and_print {| ( +1* ) |};
  [%expect {| Syntax error |}]
;;

let%expect_test "" =
  parse_and_print {| var(var) |};
  [%expect {| Syntax error |}]
;;

let%expect_test "" =
  parse_and_print {| 1var |};
  [%expect {| Syntax error |}]
;;

let%expect_test "" =
  parse_and_print {| (a) |};
  [%expect {| (Id "a") |}]
;;

let%expect_test "" =
  parse_and_print {| (( + )) |};
  [%expect {| (Id "( + )") |}]
;;

let%expect_test "" =
  parse_and_print {| (( ~- )) |};
  [%expect {| (Id "( ~- )") |}]
;;

(***************************Constant*Parser*Tests***************************)

let parse_and_print s = Parser.const_from_string s |> print_result Ast.pp_constant

let%expect_test "Constant int test" =
  parse_and_print {| 52 |};
  [%expect {| (CInt 52) |}]
;;

let%expect_test "" =
  parse_and_print {| (52) |};
  [%expect {| Syntax error |}]
;;

let%expect_test "Constant int leading zero test" =
  parse_and_print {| 052 |};
  [%expect {| (CInt 52) |}]
;;

let%expect_test "Constant string empty test" =
  parse_and_print {| "" |};
  [%expect {| (CString "") |}]
;;

let%expect_test "Constant string letters test" =
  parse_and_print {| " lEtTeRs " |};
  [%expect {| (CString " lEtTeRs ") |}]
;;

let%expect_test "Constant string digits test" =
  parse_and_print {| " 0123456789 " |};
  [%expect {| (CString " 0123456789 ") |}]
;;

let%expect_test "Constant string math symbols test" =
  parse_and_print {| " + - * / = < > ( ) & | ^ " |};
  [%expect {| (CString " + - * / = < > ( ) & | ^ ") |}]
;;

let%expect_test "Constant string grammar symbols test" =
  parse_and_print {| " . , ? ! ` : ; ' " |};
  [%expect {| (CString " . , ? ! ` : ; ' ") |}]
;;

let%expect_test "Constant string special symbols test" =
  parse_and_print {| " ~ [ ] { } # $ _ @ " |};
  [%expect {| (CString " ~ [ ] { } # $ _ @ ") |}]
;;

let%expect_test "Empty list constant" =
  parse_and_print {| [] |};
  [%expect {| CEmptyList |}]
;;

(***************************Types*Parser*Tests***************************)

let parse_and_print s = Parser.type_from_string s |> print_result Ast.pp_typ

let%expect_test "" =
  parse_and_print {| 'a * unit |};
  [%expect {| (TTuple [(TVar (Id "a")); TUnit]) |}]
;;

let%expect_test "" =
  parse_and_print {| 'a -> 'b |};
  [%expect {| (TArrow ((TVar (Id "a")), (TVar (Id "b")))) |}]
;;

let%expect_test "" =
  parse_and_print {| 'a -> 'b -> int |};
  [%expect {| (TArrow ((TVar (Id "a")), (TArrow ((TVar (Id "b")), TInt)))) |}]
;;

let%expect_test "" =
  parse_and_print {| 'a -> ('b -> 'c) -> (int * string) -> int list |};
  [%expect
    {|
    (TArrow ((TVar (Id "a")),
       (TArrow ((TArrow ((TVar (Id "b")), (TVar (Id "c")))),
          (TArrow ((TTuple [TInt; TString]), (TList TInt)))))
       )) |}]
;;

let%expect_test "" =
  parse_and_print {| (int list) list |};
  [%expect {| (TList (TList TInt)) |}]
;;

(***************************Pattern*Parser*Tests***************************)

let parse_and_print s = Parser.pattern_from_string s |> print_result Ast.pp_pattern

let%expect_test "" =
  parse_and_print {| _ |};
  [%expect {| (PVar (Id "_")) |}]
;;

let%expect_test "" =
  parse_and_print {| 52 |};
  [%expect {| (PConst (CInt 52)) |}]
;;

let%expect_test "" =
  parse_and_print {| (52) |};
  [%expect {| (PConst (CInt 52)) |}]
;;

let%expect_test "" =
  parse_and_print {| abc |};
  [%expect {| (PVar (Id "abc")) |}]
;;

let%expect_test "" =
  parse_and_print {| (abcd) |};
  [%expect {| (PVar (Id "abcd")) |}]
;;

let%expect_test "" =
  parse_and_print {| (((abcd))) |};
  [%expect {| (PVar (Id "abcd")) |}]
;;

let%expect_test "" =
  parse_and_print {| (a, "s", 5) |};
  [%expect {| (PTuple [(PVar (Id "a")); (PConst (CString "s")); (PConst (CInt 5))]) |}]
;;

let%expect_test "" =
  parse_and_print {| (a, "s", (5, 4)) |};
  [%expect
    {|
    (PTuple
       [(PVar (Id "a")); (PConst (CString "s"));
         (PTuple [(PConst (CInt 5)); (PConst (CInt 4))])]) |}]
;;

let%expect_test "" =
  parse_and_print {| 1 :: [] |};
  [%expect {| (PCons ((PConst (CInt 1)), (PConst CEmptyList))) |}]
;;

let%expect_test "" =
  parse_and_print {| (a : int) |};
  [%expect {|
    (PType ((PVar (Id "a")), TInt)) |}]
;;

let%expect_test "" =
  parse_and_print {| ((1 :: []) :: ((1 :: []) :: []) : (int list) list) |};
  [%expect
    {|
    (PType (
       (PCons ((PCons ((PConst (CInt 1)), (PConst CEmptyList))),
          (PCons ((PCons ((PConst (CInt 1)), (PConst CEmptyList))),
             (PConst CEmptyList)))
          )),
       (TList (TList TInt)))) |}]
;;

(***************************Expr*Parser*Tests***************************)

let parse_and_print s = Parser.expr_from_string s |> print_result Ast.pp_expr

let%expect_test "" =
  parse_and_print {| 1 + 2 |};
  [%expect
    {| (EApp ((EApp ((EVar (Id "( + )")), (EConst (CInt 1)))), (EConst (CInt 2)))) |}]
;;

let%expect_test "" =
  parse_and_print {| 1 + -2 |};
  [%expect
    {|
      (EApp ((EApp ((EVar (Id "( + )")), (EConst (CInt 1)))),
         (EApp ((EVar (Id "( ~- )")), (EConst (CInt 2)))))) |}]
;;

let%expect_test "" =
  parse_and_print {| 1 :: [] |};
  [%expect {|
      (ECons ((EConst (CInt 1)), (EConst CEmptyList)))
       |}]
;;

let%expect_test "" =
  parse_and_print {| [1; 2] |};
  [%expect
    {|
      (ECons ((EConst (CInt 1)), (ECons ((EConst (CInt 2)), (EConst CEmptyList))))) |}]
;;

let%expect_test "" =
  parse_and_print {| [1 :: []] |};
  [%expect
    {|
      (ECons ((ECons ((EConst (CInt 1)), (EConst CEmptyList))), (EConst CEmptyList)
         )) |}]
;;

let%expect_test "" =
  parse_and_print {| f (1 + 2) |};
  [%expect
    {|
    (EApp ((EVar (Id "f")),
       (EApp ((EApp ((EVar (Id "( + )")), (EConst (CInt 1)))), (EConst (CInt 2))
          ))
       )) |}]
;;

let%expect_test "" =
  parse_and_print {| fun n -> if 2 - n * (1 - 1 * 3 + 1)  then 5 else 0 |};
  [%expect
    {|
    (EFun ([(PVar (Id "n"))],
       (EIf (
          (EApp ((EApp ((EVar (Id "( - )")), (EConst (CInt 2)))),
             (EApp ((EApp ((EVar (Id "( * )")), (EVar (Id "n")))),
                (EApp ((EApp ((EVar (Id "( - )")), (EConst (CInt 1)))),
                   (EApp (
                      (EApp ((EVar (Id "( + )")),
                         (EApp ((EApp ((EVar (Id "( * )")), (EConst (CInt 1)))),
                            (EConst (CInt 3))))
                         )),
                      (EConst (CInt 1))))
                   ))
                ))
             )),
          (EConst (CInt 5)), (EConst (CInt 0))))
       )) |}]
;;

let%expect_test "" =
  parse_and_print {| fun x -> match x with | [] :: [] -> (f n) | a -> true | _ -> 0 |};
  [%expect
    {|
    (EFun ([(PVar (Id "x"))],
       (EMatch ((EVar (Id "x")),
          [((PCons ((PConst CEmptyList), (PConst CEmptyList))),
            (EApp ((EVar (Id "f")), (EVar (Id "n")))));
            ((PVar (Id "a")), (EConst (CBool true)));
            ((PVar (Id "_")), (EConst (CInt 0)))]
          ))
       )) |}]
;;

let%expect_test "" =
  parse_and_print {| (fun id -> id) (fun id -> id) |};
  [%expect
    {|
    (EApp ((EFun ([(PVar (Id "id"))], (EVar (Id "id")))),
       (EFun ([(PVar (Id "id"))], (EVar (Id "id")))))) |}]
;;

let%expect_test "" =
  parse_and_print
    {| fun lst ->
    (let rec helper = fun acc -> (fun lst ->
    match lst with
      | [] -> acc
      | h :: tl -> helper (h :: acc) tl)
    in helper [] lst)
   |};
  [%expect
    {|
    (EFun ([(PVar (Id "lst"))],
       (ELet (Recursive,
          ((PVar (Id "helper")),
           (EFun ([(PVar (Id "acc"))],
              (EFun ([(PVar (Id "lst"))],
                 (EMatch ((EVar (Id "lst")),
                    [((PConst CEmptyList), (EVar (Id "acc")));
                      ((PCons ((PVar (Id "h")), (PVar (Id "tl")))),
                       (EApp (
                          (EApp ((EVar (Id "helper")),
                             (ECons ((EVar (Id "h")), (EVar (Id "acc")))))),
                          (EVar (Id "tl")))))
                      ]
                    ))
                 ))
              ))),
          (EApp ((EApp ((EVar (Id "helper")), (EConst CEmptyList))),
             (EVar (Id "lst"))))
          ))
       )) |}]
;;

let%expect_test "" =
  parse_and_print {| (fun a b -> (a, (b, a))) |};
  [%expect
    {|
    (EFun ([(PVar (Id "a")); (PVar (Id "b"))],
       (ETuple [(EVar (Id "a")); (ETuple [(EVar (Id "b")); (EVar (Id "a"))])]))) |}]
;;

let%expect_test "" =
  parse_and_print {| (fun a b c -> a + b - c) |};
  [%expect
    {|
    (EFun ([(PVar (Id "a")); (PVar (Id "b")); (PVar (Id "c"))],
       (EApp (
          (EApp ((EVar (Id "( - )")),
             (EApp ((EApp ((EVar (Id "( + )")), (EVar (Id "a")))),
                (EVar (Id "b"))))
             )),
          (EVar (Id "c"))))
       )) |}]
;;

let%expect_test "" =
  parse_and_print {| (fun a :: b -> a :: b) |};
  [%expect
    {|
    (EFun ([(PCons ((PVar (Id "a")), (PVar (Id "b"))))],
       (ECons ((EVar (Id "a")), (EVar (Id "b")))))) |}]
;;

let%expect_test "" =
  parse_and_print {| (fun a b -> fun c -> a + b - c) |};
  [%expect
    {|
    (EFun ([(PVar (Id "a")); (PVar (Id "b"))],
       (EFun ([(PVar (Id "c"))],
          (EApp (
             (EApp ((EVar (Id "( - )")),
                (EApp ((EApp ((EVar (Id "( + )")), (EVar (Id "a")))),
                   (EVar (Id "b"))))
                )),
             (EVar (Id "c"))))
          ))
       )) |}]
;;

let%expect_test "" =
  parse_and_print
    {|match lst with
    | [] -> "Empty list"
    | x :: xs ->
        match x with
        | [] -> "a"
        | y :: ys -> "b" |};
  [%expect
    {|
    (EMatch ((EVar (Id "lst")),
       [((PConst CEmptyList), (EConst (CString "Empty list")));
         ((PCons ((PVar (Id "x")), (PVar (Id "xs")))),
          (EMatch ((EVar (Id "x")),
             [((PConst CEmptyList), (EConst (CString "a")));
               ((PCons ((PVar (Id "y")), (PVar (Id "ys")))),
                (EConst (CString "b")))
               ]
             )))
         ]
       )) |}]
;;

let%expect_test "" =
  parse_and_print
    {| let sum = x + y in
  let diff = x - y in
  let product =
    let intermediate = sum * diff in
    intermediate + 10
  in
  product * 2 |};
  [%expect
    {|
    (ELet (Nonrecursive,
       ((PVar (Id "sum")),
        (EApp ((EApp ((EVar (Id "( + )")), (EVar (Id "x")))), (EVar (Id "y"))))),
       (ELet (Nonrecursive,
          ((PVar (Id "diff")),
           (EApp ((EApp ((EVar (Id "( - )")), (EVar (Id "x")))), (EVar (Id "y"))
              ))),
          (ELet (Nonrecursive,
             ((PVar (Id "product")),
              (ELet (Nonrecursive,
                 ((PVar (Id "intermediate")),
                  (EApp ((EApp ((EVar (Id "( * )")), (EVar (Id "sum")))),
                     (EVar (Id "diff"))))),
                 (EApp ((EApp ((EVar (Id "( + )")), (EVar (Id "intermediate")))),
                    (EConst (CInt 10))))
                 ))),
             (EApp ((EApp ((EVar (Id "( * )")), (EVar (Id "product")))),
                (EConst (CInt 2))))
             ))
          ))
       )) |}]
;;

let%expect_test "" =
  parse_and_print {| fun x -> if if x > 4 then true else false then 1 else 0 |};
  [%expect
    {|
    (EFun ([(PVar (Id "x"))],
       (EIf (
          (EIf (
             (EApp ((EApp ((EVar (Id "( > )")), (EVar (Id "x")))),
                (EConst (CInt 4)))),
             (EConst (CBool true)), (EConst (CBool false)))),
          (EConst (CInt 1)), (EConst (CInt 0))))
       )) |}]
;;

let%expect_test "" =
  parse_and_print {| (1 + 2 : int) |};
  [%expect
    {|
      (EType (
         (EApp ((EApp ((EVar (Id "( + )")), (EConst (CInt 1)))), (EConst (CInt 2))
            )),
         TInt)) |}]
;;

let%expect_test "" =
  parse_and_print {| fun f g x -> (f x : int) |};
  [%expect
    {|
      (EFun ([(PVar (Id "f")); (PVar (Id "g")); (PVar (Id "x"))],
         (EType ((EApp ((EVar (Id "f")), (EVar (Id "x")))), TInt)))) |}]
;;

let%expect_test "" =
  parse_and_print {| (fun x -> (x: int) : int -> int) |};
  [%expect
    {|
      (EType ((EFun ([(PVar (Id "x"))], (EType ((EVar (Id "x")), TInt)))),
         (TArrow (TInt, TInt)))) |}]
;;

let%expect_test "" =
  parse_and_print {| let a = (fun x -> (x: int) : int -> int) in a |};
  [%expect
    {|
      (ELet (Nonrecursive,
         ((PVar (Id "a")),
          (EType ((EFun ([(PVar (Id "x"))], (EType ((EVar (Id "x")), TInt)))),
             (TArrow (TInt, TInt))))),
         (EVar (Id "a")))) |}]
;;

(***************************Structure*Item*Parser*Tests***************************)

let parse_and_print s =
  Parser.structure_item_from_string s |> print_result Ast.pp_structure_item
;;

let%expect_test "" =
  parse_and_print {| let aBC = fun n -> if n then 5 else 0 |};
  [%expect
    {|
    (SILet (Nonrecursive,
       [((PVar (Id "aBC")),
         (EFun ([(PVar (Id "n"))],
            (EIf ((EVar (Id "n")), (EConst (CInt 5)), (EConst (CInt 0)))))))
         ]
       )) |}]
;;

let%expect_test "" =
  parse_and_print {| let rec a = 5 and b = 4;; |};
  [%expect
    {|
    (SILet (Recursive,
       [((PVar (Id "a")), (EConst (CInt 5)));
         ((PVar (Id "b")), (EConst (CInt 4)))]
       )) |}]
;;

let%expect_test "" =
  parse_and_print {| let rec fix = fun f -> (fun x -> f (fix x) x) |};
  [%expect
    {|
    (SILet (Recursive,
       [((PVar (Id "fix")),
         (EFun ([(PVar (Id "f"))],
            (EFun ([(PVar (Id "x"))],
               (EApp (
                  (EApp ((EVar (Id "f")),
                     (EApp ((EVar (Id "fix")), (EVar (Id "x")))))),
                  (EVar (Id "x"))))
               ))
            )))
         ]
       )) |}]
;;

let%expect_test _ =
  let _ = parse_and_print {| 2 + "a" |} in
  [%expect
    {|
    (SIExpr
       (EApp ((EApp ((EVar (Id "( + )")), (EConst (CInt 2)))),
          (EConst (CString "a"))))) |}]
;;

(***************************Structure*Parser*Tests***************************)

let parse_and_print s = Parser.structure_from_string s |> print_result Ast.pp_structure

let%expect_test "" =
  parse_and_print {| let rec fix = 4;;     |};
  [%expect {|
    [(SILet (Recursive, [((PVar (Id "fix")), (EConst (CInt 4)))]))] |}]
;;

let%expect_test "" =
  parse_and_print {| let a = 5  |};
  [%expect {|
    [(SILet (Nonrecursive, [((PVar (Id "a")), (EConst (CInt 5)))]))] |}]
;;

let%expect_test "" =
  parse_and_print {| let (a, b, _) = 1, 2, 3;;  |};
  [%expect
    {|
    [(SILet (Nonrecursive,
        [((PTuple [(PVar (Id "a")); (PVar (Id "b")); (PVar (Id "_"))]),
          (ETuple [(EConst (CInt 1)); (EConst (CInt 2)); (EConst (CInt 3))]))]
        ))
      ] |}]
;;

let%expect_test "" =
  parse_and_print
    {| let rec is_even n =
    if n = 0 then true
    else is_odd (n - 1)
  
  and is_odd n =
    if n = 0 then false
    else is_even (n - 1);;  |};
  [%expect
    {|
    [(SILet (Recursive,
        [((PVar (Id "is_even")),
          (EFun ([(PVar (Id "n"))],
             (EIf (
                (EApp ((EApp ((EVar (Id "( = )")), (EVar (Id "n")))),
                   (EConst (CInt 0)))),
                (EConst (CBool true)),
                (EApp ((EVar (Id "is_odd")),
                   (EApp ((EApp ((EVar (Id "( - )")), (EVar (Id "n")))),
                      (EConst (CInt 1))))
                   ))
                ))
             )));
          ((PVar (Id "is_odd")),
           (EFun ([(PVar (Id "n"))],
              (EIf (
                 (EApp ((EApp ((EVar (Id "( = )")), (EVar (Id "n")))),
                    (EConst (CInt 0)))),
                 (EConst (CBool false)),
                 (EApp ((EVar (Id "is_even")),
                    (EApp ((EApp ((EVar (Id "( - )")), (EVar (Id "n")))),
                       (EConst (CInt 1))))
                    ))
                 ))
              )))
          ]
        ))
      ] |}]
;;

let%expect_test "" =
  parse_and_print {| let (a, b, _) = 1, 2, 3 and c = "s"  |};
  [%expect
    {|
    [(SILet (Nonrecursive,
        [((PTuple [(PVar (Id "a")); (PVar (Id "b")); (PVar (Id "_"))]),
          (ETuple [(EConst (CInt 1)); (EConst (CInt 2)); (EConst (CInt 3))]));
          ((PVar (Id "c")), (EConst (CString "s")))]
        ))
      ] |}]
;;

let%expect_test "" =
  parse_and_print {| let () = 5  |};
  [%expect {|
    [(SILet (Nonrecursive, [((PConst CUnit), (EConst (CInt 5)))]))] |}]
;;

let%expect_test "" =
  parse_and_print {| let ( + ) a b = a - b;;     |};
  [%expect
    {|
    [(SILet (Nonrecursive,
        [((PVar (Id "( + )")),
          (EFun ([(PVar (Id "a")); (PVar (Id "b"))],
             (EApp ((EApp ((EVar (Id "( - )")), (EVar (Id "a")))),
                (EVar (Id "b"))))
             )))
          ]
        ))
      ] |}]
;;

let%expect_test "" =
  parse_and_print {| let rec (a,b) = (a,b) |};
  [%expect
    {|
    [(SILet (Recursive,
        [((PTuple [(PVar (Id "a")); (PVar (Id "b"))]),
          (ETuple [(EVar (Id "a")); (EVar (Id "b"))]))]
        ))
      ]
     |}]
;;

let%expect_test "" =
  parse_and_print {| let rec (a,b) = a, (b, c) |};
  [%expect
    {|
    [(SILet (Recursive,
        [((PTuple [(PVar (Id "a")); (PVar (Id "b"))]),
          (ETuple [(EVar (Id "a")); (ETuple [(EVar (Id "b")); (EVar (Id "c"))])]))
          ]
        ))
      ]
     |}]
;;

let%expect_test "" =
  parse_and_print {| let rec a b c = 5 |};
  [%expect
    {|
    [(SILet (Recursive,
        [((PVar (Id "a")),
          (EFun ([(PVar (Id "b")); (PVar (Id "c"))], (EConst (CInt 5)))))]
        ))
      ]
     |}]
;;

let%expect_test "" =
  parse_and_print {| let a = fun x -> if (let b = 4 in b > x) then true else false |};
  [%expect
    {|
    [(SILet (Nonrecursive,
        [((PVar (Id "a")),
          (EFun ([(PVar (Id "x"))],
             (EIf (
                (ELet (Nonrecursive, ((PVar (Id "b")), (EConst (CInt 4))),
                   (EApp ((EApp ((EVar (Id "( > )")), (EVar (Id "b")))),
                      (EVar (Id "x"))))
                   )),
                (EConst (CBool true)), (EConst (CBool false))))
             )))
          ]
        ))
      ]
     |}]
;;

let%expect_test "" =
  parse_and_print {| let a = (let b = 5 in b) + (let c = 6 in c) |};
  [%expect
    {|
    [(SILet (Nonrecursive,
        [((PVar (Id "a")),
          (EApp (
             (EApp ((EVar (Id "( + )")),
                (ELet (Nonrecursive, ((PVar (Id "b")), (EConst (CInt 5))),
                   (EVar (Id "b"))))
                )),
             (ELet (Nonrecursive, ((PVar (Id "c")), (EConst (CInt 6))),
                (EVar (Id "c"))))
             )))
          ]
        ))
      ]
     |}]
;;

let%expect_test "" =
  parse_and_print
    {| ;;
  
  2 + 3;;
  let a = 5;;
;;
;;

  let rec a = 5 and b = 6;;

  let rec fix = fun f x -> f (fix f) x
  let fac = fix (fun self n -> if n <= 1 then 1 else n * self (n - 1))
|};
  [%expect
    {|
    [(SIExpr
        (EApp ((EApp ((EVar (Id "( + )")), (EConst (CInt 2)))), (EConst (CInt 3))
           )));
      (SILet (Nonrecursive, [((PVar (Id "a")), (EConst (CInt 5)))]));
      (SILet (Recursive,
         [((PVar (Id "a")), (EConst (CInt 5)));
           ((PVar (Id "b")), (EConst (CInt 6)))]
         ));
      (SILet (Recursive,
         [((PVar (Id "fix")),
           (EFun ([(PVar (Id "f")); (PVar (Id "x"))],
              (EApp (
                 (EApp ((EVar (Id "f")),
                    (EApp ((EVar (Id "fix")), (EVar (Id "f")))))),
                 (EVar (Id "x"))))
              )))
           ]
         ));
      (SILet (Nonrecursive,
         [((PVar (Id "fac")),
           (EApp ((EVar (Id "fix")),
              (EFun ([(PVar (Id "self")); (PVar (Id "n"))],
                 (EIf (
                    (EApp ((EApp ((EVar (Id "( <= )")), (EVar (Id "n")))),
                       (EConst (CInt 1)))),
                    (EConst (CInt 1)),
                    (EApp ((EApp ((EVar (Id "( * )")), (EVar (Id "n")))),
                       (EApp ((EVar (Id "self")),
                          (EApp ((EApp ((EVar (Id "( - )")), (EVar (Id "n")))),
                             (EConst (CInt 1))))
                          ))
                       ))
                    ))
                 ))
              )))
           ]
         ))
      ]
     |}]
;;

let%expect_test "" =
  parse_and_print {| let a (b: bool * string) (c: int -> int): int = 5;; |};
  [%expect
    {|
    [(SILet (Nonrecursive,
        [((PType ((PVar (Id "a")),
             (TArrow ((TTuple [TBool; TString]),
                (TArrow ((TArrow (TInt, TInt)), TInt))))
             )),
          (EFun (
             [(PType ((PVar (Id "b")), (TTuple [TBool; TString])));
               (PType ((PVar (Id "c")), (TArrow (TInt, TInt))))],
             (EConst (CInt 5)))))
          ]
        ))
      ]
     |}]
;;

let%expect_test "" =
  parse_and_print {| let a b c: int = 5;; |};
  [%expect
    {|
    [(SILet (Nonrecursive,
        [((PType ((PVar (Id "a")),
             (TArrow ((TVar (Id "Var0")), (TArrow ((TVar (Id "Var1")), TInt)))))),
          (EFun ([(PVar (Id "b")); (PVar (Id "c"))], (EConst (CInt 5)))))]
        ))
      ]
     |}]
;;

let%expect_test "" =
  parse_and_print {| let a b : int -> int = fun x -> x + b;; |};
  [%expect
    {|
    [(SILet (Nonrecursive,
        [((PType ((PVar (Id "a")),
             (TArrow ((TVar (Id "Var0")), (TArrow (TInt, TInt)))))),
          (EFun ([(PVar (Id "b"))],
             (EFun ([(PVar (Id "x"))],
                (EApp ((EApp ((EVar (Id "( + )")), (EVar (Id "x")))),
                   (EVar (Id "b"))))
                ))
             )))
          ]
        ))
      ]
     |}]
;;

let%expect_test "" =
  parse_and_print {|let map p = let (a,b) = p in a + b |};
  [%expect
    {|       
  [(SILet (Nonrecursive,
      [((PVar (Id "map")),
        (EFun ([(PVar (Id "p"))],
           (ELet (Nonrecursive,
              ((PTuple [(PVar (Id "a")); (PVar (Id "b"))]), (EVar (Id "p"))),
              (EApp ((EApp ((EVar (Id "( + )")), (EVar (Id "a")))),
                 (EVar (Id "b"))))
              ))
           )))
        ]
      ))
    ] |}]
;;

let%expect_test "" =
  parse_and_print {|fun x () x -> x |};
  [%expect
    {|       
  [(SIExpr
      (EFun ([(PVar (Id "x")); (PConst CUnit); (PVar (Id "x"))],
         (EVar (Id "x")))))
    ] |}]
;;

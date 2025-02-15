(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

(* Fun expression ([PVar] pattern) *)

let%expect_test _ =
  parse_expression {| fun x -> x |};
  [%expect {| (EFun (((PVar (Id "x")), []), (EIdentifier (Id "x")))) |}]
;;

let%expect_test _ =
  parse_expression {| fun x -> y |};
  [%expect {| (EFun (((PVar (Id "x")), []), (EIdentifier (Id "y")))) |}]
;;

let%expect_test _ =
  parse_expression {| fun x y z -> x + y + z |};
  [%expect
    {|
    (EFun (((PVar (Id "x")), [(PVar (Id "y")); (PVar (Id "z"))]),
       (EApplication ((EIdentifier (Id "( + )")),
          (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
             [(EIdentifier (Id "y"))])),
          [(EIdentifier (Id "z"))]))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| fun x -> fun y -> fun z -> x + y + z |};
  [%expect
    {|
    (EFun (((PVar (Id "x")), []),
       (EFun (((PVar (Id "y")), []),
          (EFun (((PVar (Id "z")), []),
             (EApplication ((EIdentifier (Id "( + )")),
                (EApplication ((EIdentifier (Id "( + )")),
                   (EIdentifier (Id "x")), [(EIdentifier (Id "y"))])),
                [(EIdentifier (Id "z"))]))
             ))
          ))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| fun x -> fun y -> x + y |};
  [%expect
    {|
    (EFun (((PVar (Id "x")), []),
       (EFun (((PVar (Id "y")), []),
          (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
             [(EIdentifier (Id "y"))]))
          ))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| fun x -> let y = x * 2 in y + x |};
  [%expect
    {|
    (EFun (((PVar (Id "x")), []),
       (ELetIn (
          ((PVar (Id "y")),
           (EApplication ((EIdentifier (Id "( * )")), (EIdentifier (Id "x")),
              [(EConstant (CInt 2))]))),
          [],
          (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "y")),
             [(EIdentifier (Id "x"))]))
          ))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| fun x -> if x > 0 then x else x - 1 |};
  [%expect
    {|
    (EFun (((PVar (Id "x")), []),
       (EIfThenElse (
          (EApplication ((EIdentifier (Id "( > )")), (EIdentifier (Id "x")),
             [(EConstant (CInt 0))])),
          (EIdentifier (Id "x")),
          (Some (EApplication ((EIdentifier (Id "( - )")),
                   (EIdentifier (Id "x")), [(EConstant (CInt 1))])))
          ))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| fun x -> let y = x + 1 in let z = y * 2 in z |};
  [%expect
    {|
    (EFun (((PVar (Id "x")), []),
       (ELetIn (
          ((PVar (Id "y")),
           (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
              [(EConstant (CInt 1))]))),
          [],
          (ELetIn (
             ((PVar (Id "z")),
              (EApplication ((EIdentifier (Id "( * )")), (EIdentifier (Id "y")),
                 [(EConstant (CInt 2))]))),
             [], (EIdentifier (Id "z"))))
          ))
       )) |}]
;;

(* ---------------- *)

(* Fun expression (complex patterns) *)

let%expect_test _ =
  parse_expression {| fun (x, y) -> x + y |};
  [%expect
    {|
    (EFun (((PTuple ((PVar (Id "x")), (PVar (Id "y")), [])), []),
       (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
          [(EIdentifier (Id "y"))]))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| fun (x :: y) -> x + y |};
  [%expect
    {|
    (EFun (((PListConstructor ((PVar (Id "x")), (PVar (Id "y")))), []),
       (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
          [(EIdentifier (Id "y"))]))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| fun (a, b) c d e (x :: y) -> a + b + c + d + e + x + y |};
  [%expect
    {|
    (EFun (
       ((PTuple ((PVar (Id "a")), (PVar (Id "b")), [])),
        [(PVar (Id "c")); (PVar (Id "d")); (PVar (Id "e"));
          (PListConstructor ((PVar (Id "x")), (PVar (Id "y"))))]),
       (EApplication ((EIdentifier (Id "( + )")),
          (EApplication ((EIdentifier (Id "( + )")),
             (EApplication ((EIdentifier (Id "( + )")),
                (EApplication ((EIdentifier (Id "( + )")),
                   (EApplication ((EIdentifier (Id "( + )")),
                      (EApplication ((EIdentifier (Id "( + )")),
                         (EIdentifier (Id "a")), [(EIdentifier (Id "b"))])),
                      [(EIdentifier (Id "c"))])),
                   [(EIdentifier (Id "d"))])),
                [(EIdentifier (Id "e"))])),
             [(EIdentifier (Id "x"))])),
          [(EIdentifier (Id "y"))]))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| fun ((x, y) :: xs) -> x + y |};
  [%expect
    {|
    (EFun (
       ((PListConstructor ((PTuple ((PVar (Id "x")), (PVar (Id "y")), [])),
           (PVar (Id "xs")))),
        []),
       (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
          [(EIdentifier (Id "y"))]))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| fun ((a, (b, c)), d) -> a + b + c + d |};
  [%expect
    {|
    (EFun (
       ((PTuple (
           (PTuple ((PVar (Id "a")),
              (PTuple ((PVar (Id "b")), (PVar (Id "c")), [])), [])),
           (PVar (Id "d")), [])),
        []),
       (EApplication ((EIdentifier (Id "( + )")),
          (EApplication ((EIdentifier (Id "( + )")),
             (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "a")),
                [(EIdentifier (Id "b"))])),
             [(EIdentifier (Id "c"))])),
          [(EIdentifier (Id "d"))]))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| fun ((x, y), z :: zs) -> x + y + z |};
  [%expect
    {|
    (EFun (
       ((PTuple ((PTuple ((PVar (Id "x")), (PVar (Id "y")), [])),
           (PListConstructor ((PVar (Id "z")), (PVar (Id "zs")))), [])),
        []),
       (EApplication ((EIdentifier (Id "( + )")),
          (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
             [(EIdentifier (Id "y"))])),
          [(EIdentifier (Id "z"))]))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| fun (x, _) -> x |};
  [%expect
    {|
    (EFun (((PTuple ((PVar (Id "x")), PAny, [])), []), (EIdentifier (Id "x")))) |}]
;;

let%expect_test _ =
  parse_expression {| fun x (y, z) _ (_, _) -> x |};
  [%expect
    {|
    (EFun (
       ((PVar (Id "x")),
        [(PTuple ((PVar (Id "y")), (PVar (Id "z")), [])); PAny;
          (PTuple (PAny, PAny, []))]),
       (EIdentifier (Id "x")))) |}]
;;

let%expect_test _ =
  parse_expression {| fun x, y -> x + y |};
  [%expect {| Syntax error. |}]
;;

let%expect_test _ =
  parse_expression {| fun x :: y -> x + y |};
  [%expect {| Syntax error. |}]
;;

(* ---------------- *)

(* Basic let in expression ([PVar] pattern) *)

let%expect_test _ =
  parse_expression {| let x = 5 in x |};
  [%expect
    {|
    (ELetIn (((PVar (Id "x")), (EConstant (CInt 5))), [], (EIdentifier (Id "x"))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| let y = true in y |};
  [%expect
    {|
    (ELetIn (((PVar (Id "y")), (EConstant (CBool true))), [],
       (EIdentifier (Id "y")))) |}]
;;

let%expect_test _ =
  parse_expression {| let f x = x in (f 1, f true) |};
  [%expect
    {|
    (ELetIn (
       ((PVar (Id "f")), (EFun (((PVar (Id "x")), []), (EIdentifier (Id "x"))))),
       [],
       (ETuple (
          (EApplication ((EIdentifier (Id "f")), (EConstant (CInt 1)), [])),
          (EApplication ((EIdentifier (Id "f")), (EConstant (CBool true)), [])),
          []))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| let f x y = (x, y) in (f 1 true, f 2 false) |};
  [%expect
    {|
    (ELetIn (
       ((PVar (Id "f")),
        (EFun (((PVar (Id "x")), [(PVar (Id "y"))]),
           (ETuple ((EIdentifier (Id "x")), (EIdentifier (Id "y")), []))))),
       [],
       (ETuple (
          (EApplication ((EIdentifier (Id "f")), (EConstant (CInt 1)),
             [(EConstant (CBool true))])),
          (EApplication ((EIdentifier (Id "f")), (EConstant (CInt 2)),
             [(EConstant (CBool false))])),
          []))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| let x = 3 in let y = 4 in x * y |};
  [%expect
    {|
    (ELetIn (((PVar (Id "x")), (EConstant (CInt 3))), [],
       (ELetIn (((PVar (Id "y")), (EConstant (CInt 4))), [],
          (EApplication ((EIdentifier (Id "( * )")), (EIdentifier (Id "x")),
             [(EIdentifier (Id "y"))]))
          ))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| let x = 2 in let y = x + 3 in let z = y * 2 in z |};
  [%expect
    {|
    (ELetIn (((PVar (Id "x")), (EConstant (CInt 2))), [],
       (ELetIn (
          ((PVar (Id "y")),
           (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
              [(EConstant (CInt 3))]))),
          [],
          (ELetIn (
             ((PVar (Id "z")),
              (EApplication ((EIdentifier (Id "( * )")), (EIdentifier (Id "y")),
                 [(EConstant (CInt 2))]))),
             [], (EIdentifier (Id "z"))))
          ))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| let x = 5 in if x > 3 then x else 0 |};
  [%expect
    {|
    (ELetIn (((PVar (Id "x")), (EConstant (CInt 5))), [],
       (EIfThenElse (
          (EApplication ((EIdentifier (Id "( > )")), (EIdentifier (Id "x")),
             [(EConstant (CInt 3))])),
          (EIdentifier (Id "x")), (Some (EConstant (CInt 0)))))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| let f = fun x -> x + 1 in f 10 |};
  [%expect
    {|
    (ELetIn (
       ((PVar (Id "f")),
        (EFun (((PVar (Id "x")), []),
           (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
              [(EConstant (CInt 1))]))
           ))),
       [], (EApplication ((EIdentifier (Id "f")), (EConstant (CInt 10)), [])))) |}]
;;

let%expect_test _ =
  parse_expression {| let x = 5 in let y = let z = x + 2 in z * 2 in y |};
  [%expect
    {|
    (ELetIn (((PVar (Id "x")), (EConstant (CInt 5))), [],
       (ELetIn (
          ((PVar (Id "y")),
           (ELetIn (
              ((PVar (Id "z")),
               (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
                  [(EConstant (CInt 2))]))),
              [],
              (EApplication ((EIdentifier (Id "( * )")), (EIdentifier (Id "z")),
                 [(EConstant (CInt 2))]))
              ))),
          [], (EIdentifier (Id "y"))))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| let x = 10 in let y = x + x in y |};
  [%expect
    {|
    (ELetIn (((PVar (Id "x")), (EConstant (CInt 10))), [],
       (ELetIn (
          ((PVar (Id "y")),
           (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
              [(EIdentifier (Id "x"))]))),
          [], (EIdentifier (Id "y"))))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| let x = 3 in let y = x + 2 in let z = y * x in z - y |};
  [%expect
    {|
    (ELetIn (((PVar (Id "x")), (EConstant (CInt 3))), [],
       (ELetIn (
          ((PVar (Id "y")),
           (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
              [(EConstant (CInt 2))]))),
          [],
          (ELetIn (
             ((PVar (Id "z")),
              (EApplication ((EIdentifier (Id "( * )")), (EIdentifier (Id "y")),
                 [(EIdentifier (Id "x"))]))),
             [],
             (EApplication ((EIdentifier (Id "( - )")), (EIdentifier (Id "z")),
                [(EIdentifier (Id "y"))]))
             ))
          ))
       )) |}]
;;

(* ---------------- *)

(* Basic let in expression (complex patterns) *)

let%expect_test _ =
  parse_expression {| let x, y = (1, 2) in x + y |};
  [%expect
    {|
    (ELetIn (
       ((PTuple ((PVar (Id "x")), (PVar (Id "y")), [])),
        (ETuple ((EConstant (CInt 1)), (EConstant (CInt 2)), []))),
       [],
       (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
          [(EIdentifier (Id "y"))]))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| let x :: xs = [1; 2; 3] in x |};
  [%expect
    {|
    (ELetIn (
       ((PListConstructor ((PVar (Id "x")), (PVar (Id "xs")))),
        (EListConstructor ((EConstant (CInt 1)),
           (EListConstructor ((EConstant (CInt 2)),
              (EListConstructor ((EConstant (CInt 3)), EEmptyList))))
           ))),
       [], (EIdentifier (Id "x")))) |}]
;;

let%expect_test _ =
  parse_expression {| let ((a, b), c) = ((1, 2), 3) in a + b + c |};
  [%expect
    {|
    (ELetIn (
       ((PTuple ((PTuple ((PVar (Id "a")), (PVar (Id "b")), [])),
           (PVar (Id "c")), [])),
        (ETuple ((ETuple ((EConstant (CInt 1)), (EConstant (CInt 2)), [])),
           (EConstant (CInt 3)), []))),
       [],
       (EApplication ((EIdentifier (Id "( + )")),
          (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "a")),
             [(EIdentifier (Id "b"))])),
          [(EIdentifier (Id "c"))]))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| let (x, _) = (1, 2) in x |};
  [%expect
    {|
    (ELetIn (
       ((PTuple ((PVar (Id "x")), PAny, [])),
        (ETuple ((EConstant (CInt 1)), (EConstant (CInt 2)), []))),
       [], (EIdentifier (Id "x")))) |}]
;;

let%expect_test _ =
  parse_expression {| let ((x, y), z :: zs) = ((1, 2), [3; 4]) in x + y + z |};
  [%expect
    {|
    (ELetIn (
       ((PTuple ((PTuple ((PVar (Id "x")), (PVar (Id "y")), [])),
           (PListConstructor ((PVar (Id "z")), (PVar (Id "zs")))), [])),
        (ETuple ((ETuple ((EConstant (CInt 1)), (EConstant (CInt 2)), [])),
           (EListConstructor ((EConstant (CInt 3)),
              (EListConstructor ((EConstant (CInt 4)), EEmptyList)))),
           []))),
       [],
       (EApplication ((EIdentifier (Id "( + )")),
          (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
             [(EIdentifier (Id "y"))])),
          [(EIdentifier (Id "z"))]))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| let f (x, y) (w :: u) k = x + y + w + u + k |};
  [%expect {|
    Syntax error. |}]
;;

(* ---------------- *)

(* Basic let in expression with and ([PVar] pattern) *)

let%expect_test _ =
  parse_expression {| let x = 1 and y = 2 in x + y |};
  [%expect
    {|
    (ELetIn (((PVar (Id "x")), (EConstant (CInt 1))),
       [((PVar (Id "y")), (EConstant (CInt 2)))],
       (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
          [(EIdentifier (Id "y"))]))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| let x = 3 and y = 4 in if x > y then x else y |};
  [%expect
    {|
    (ELetIn (((PVar (Id "x")), (EConstant (CInt 3))),
       [((PVar (Id "y")), (EConstant (CInt 4)))],
       (EIfThenElse (
          (EApplication ((EIdentifier (Id "( > )")), (EIdentifier (Id "x")),
             [(EIdentifier (Id "y"))])),
          (EIdentifier (Id "x")), (Some (EIdentifier (Id "y")))))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| let f x = x and y = 4 in f y |};
  [%expect
    {|
    (ELetIn (
       ((PVar (Id "f")), (EFun (((PVar (Id "x")), []), (EIdentifier (Id "x"))))),
       [((PVar (Id "y")), (EConstant (CInt 4)))],
       (EApplication ((EIdentifier (Id "f")), (EIdentifier (Id "y")), [])))) |}]
;;

(* ---------------- *)

(* Basic let in expression with and (complex patterns) *)

let%expect_test _ =
  parse_expression {| let (x, y) = (1, 2) and (a, b) = (3, 4) in x + a |};
  [%expect
    {|
    (ELetIn (
       ((PTuple ((PVar (Id "x")), (PVar (Id "y")), [])),
        (ETuple ((EConstant (CInt 1)), (EConstant (CInt 2)), []))),
       [((PTuple ((PVar (Id "a")), (PVar (Id "b")), [])),
         (ETuple ((EConstant (CInt 3)), (EConstant (CInt 4)), [])))],
       (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
          [(EIdentifier (Id "a"))]))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| let x :: xs = [1; 2; 3] and y = 4 in x + y |};
  [%expect
    {|
    (ELetIn (
       ((PListConstructor ((PVar (Id "x")), (PVar (Id "xs")))),
        (EListConstructor ((EConstant (CInt 1)),
           (EListConstructor ((EConstant (CInt 2)),
              (EListConstructor ((EConstant (CInt 3)), EEmptyList))))
           ))),
       [((PVar (Id "y")), (EConstant (CInt 4)))],
       (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
          [(EIdentifier (Id "y"))]))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| let (x, y) :: xs = [(1, 2); (3, 4)] and z = 5 in x + y + z |};
  [%expect
    {|
    (ELetIn (
       ((PListConstructor ((PTuple ((PVar (Id "x")), (PVar (Id "y")), [])),
           (PVar (Id "xs")))),
        (EListConstructor (
           (ETuple ((EConstant (CInt 1)), (EConstant (CInt 2)), [])),
           (EListConstructor (
              (ETuple ((EConstant (CInt 3)), (EConstant (CInt 4)), [])),
              EEmptyList))
           ))),
       [((PVar (Id "z")), (EConstant (CInt 5)))],
       (EApplication ((EIdentifier (Id "( + )")),
          (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
             [(EIdentifier (Id "y"))])),
          [(EIdentifier (Id "z"))]))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| let x :: _ = [1; 2; 3] and y = 4 in x + y |};
  [%expect
    {|
    (ELetIn (
       ((PListConstructor ((PVar (Id "x")), PAny)),
        (EListConstructor ((EConstant (CInt 1)),
           (EListConstructor ((EConstant (CInt 2)),
              (EListConstructor ((EConstant (CInt 3)), EEmptyList))))
           ))),
       [((PVar (Id "y")), (EConstant (CInt 4)))],
       (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
          [(EIdentifier (Id "y"))]))
       )) |}]
;;

(* ---------------- *)

(* Recursive let in expression *)

let%expect_test _ =
  parse_expression {| let rec f x = if x = 0 then 1 else x * f (x - 1) |};
  [%expect {|
    Syntax error. |}]
;;

let%expect_test _ =
  parse_expression
    {| let rec sum lst = match lst with [] -> 0 | x :: xs -> x + sum xs in sum [1; 2; 3; 4] |};
  [%expect
    {|
    (ERecLetIn (
       ((PVar (Id "sum")),
        (EFun (((PVar (Id "lst")), []),
           (EMatchWith ((EIdentifier (Id "lst")), (PNill, (EConstant (CInt 0))),
              [((PListConstructor ((PVar (Id "x")), (PVar (Id "xs")))),
                (EApplication ((EIdentifier (Id "( + )")),
                   (EIdentifier (Id "x")),
                   [(EApplication ((EIdentifier (Id "sum")),
                       (EIdentifier (Id "xs")), []))
                     ]
                   )))
                ]
              ))
           ))),
       [],
       (EApplication ((EIdentifier (Id "sum")),
          (EListConstructor ((EConstant (CInt 1)),
             (EListConstructor ((EConstant (CInt 2)),
                (EListConstructor ((EConstant (CInt 3)),
                   (EListConstructor ((EConstant (CInt 4)), EEmptyList))))
                ))
             )),
          []))
       )) |}]
;;

let%expect_test _ =
  parse_expression
    {| let rec length lst = match lst with [] -> 0 | _ :: xs -> 1 + length xs in length [1; 2; 3] |};
  [%expect
    {|
    (ERecLetIn (
       ((PVar (Id "length")),
        (EFun (((PVar (Id "lst")), []),
           (EMatchWith ((EIdentifier (Id "lst")), (PNill, (EConstant (CInt 0))),
              [((PListConstructor (PAny, (PVar (Id "xs")))),
                (EApplication ((EIdentifier (Id "( + )")), (EConstant (CInt 1)),
                   [(EApplication ((EIdentifier (Id "length")),
                       (EIdentifier (Id "xs")), []))
                     ]
                   )))
                ]
              ))
           ))),
       [],
       (EApplication ((EIdentifier (Id "length")),
          (EListConstructor ((EConstant (CInt 1)),
             (EListConstructor ((EConstant (CInt 2)),
                (EListConstructor ((EConstant (CInt 3)), EEmptyList))))
             )),
          []))
       )) |}]
;;

let%expect_test _ =
  parse_expression
    {| let rec merge lst1 lst2 = match (lst1, lst2) with 
    | ([], lst) -> lst 
    | (lst, []) -> lst 
    | (x :: xs, y :: ys) -> if x < y then x :: merge xs (y :: ys) else y :: merge (x :: xs) ys 
  in merge [1; 3; 5] [2; 4; 6] |};
  [%expect
    {|
    (ERecLetIn (
       ((PVar (Id "merge")),
        (EFun (((PVar (Id "lst1")), [(PVar (Id "lst2"))]),
           (EMatchWith (
              (ETuple ((EIdentifier (Id "lst1")), (EIdentifier (Id "lst2")), [])),
              ((PTuple (PNill, (PVar (Id "lst")), [])), (EIdentifier (Id "lst"))),
              [((PTuple ((PVar (Id "lst")), PNill, [])), (EIdentifier (Id "lst")));
                ((PTuple ((PListConstructor ((PVar (Id "x")), (PVar (Id "xs")))),
                    (PListConstructor ((PVar (Id "y")), (PVar (Id "ys")))),
                    [])),
                 (EIfThenElse (
                    (EApplication ((EIdentifier (Id "( < )")),
                       (EIdentifier (Id "x")), [(EIdentifier (Id "y"))])),
                    (EListConstructor ((EIdentifier (Id "x")),
                       (EApplication ((EIdentifier (Id "merge")),
                          (EIdentifier (Id "xs")),
                          [(EListConstructor ((EIdentifier (Id "y")),
                              (EIdentifier (Id "ys"))))
                            ]
                          ))
                       )),
                    (Some (EListConstructor ((EIdentifier (Id "y")),
                             (EApplication ((EIdentifier (Id "merge")),
                                (EListConstructor ((EIdentifier (Id "x")),
                                   (EIdentifier (Id "xs")))),
                                [(EIdentifier (Id "ys"))]))
                             )))
                    )))
                ]
              ))
           ))),
       [],
       (EApplication ((EIdentifier (Id "merge")),
          (EListConstructor ((EConstant (CInt 1)),
             (EListConstructor ((EConstant (CInt 3)),
                (EListConstructor ((EConstant (CInt 5)), EEmptyList))))
             )),
          [(EListConstructor ((EConstant (CInt 2)),
              (EListConstructor ((EConstant (CInt 4)),
                 (EListConstructor ((EConstant (CInt 6)), EEmptyList))))
              ))
            ]
          ))
       )) |}]
;;

(* ---------------- *)

(* Let in expression with mutual recursion *)

let%expect_test _ =
  parse_expression {| 
    let rec f x = g x
    and g x = f x
    in
    (f, g)
  |};
  [%expect
    {|
    (ERecLetIn (
       ((PVar (Id "f")),
        (EFun (((PVar (Id "x")), []),
           (EApplication ((EIdentifier (Id "g")), (EIdentifier (Id "x")), []))))),
       [((PVar (Id "g")),
         (EFun (((PVar (Id "x")), []),
            (EApplication ((EIdentifier (Id "f")), (EIdentifier (Id "x")), [])))))
         ],
       (ETuple ((EIdentifier (Id "f")), (EIdentifier (Id "g")), [])))) |}]
;;

let%expect_test _ =
  parse_expression
    {| 
    let rec is_even n = if n = 0 then true else is_odd (n - 1) 
    and is_odd n = if n = 0 then false else is_even (n - 1) 
    in is_even 4
  |};
  [%expect
    {|
    (ERecLetIn (
       ((PVar (Id "is_even")),
        (EFun (((PVar (Id "n")), []),
           (EIfThenElse (
              (EApplication ((EIdentifier (Id "( = )")), (EIdentifier (Id "n")),
                 [(EConstant (CInt 0))])),
              (EConstant (CBool true)),
              (Some (EApplication ((EIdentifier (Id "is_odd")),
                       (EApplication ((EIdentifier (Id "( - )")),
                          (EIdentifier (Id "n")), [(EConstant (CInt 1))])),
                       [])))
              ))
           ))),
       [((PVar (Id "is_odd")),
         (EFun (((PVar (Id "n")), []),
            (EIfThenElse (
               (EApplication ((EIdentifier (Id "( = )")), (EIdentifier (Id "n")),
                  [(EConstant (CInt 0))])),
               (EConstant (CBool false)),
               (Some (EApplication ((EIdentifier (Id "is_even")),
                        (EApplication ((EIdentifier (Id "( - )")),
                           (EIdentifier (Id "n")), [(EConstant (CInt 1))])),
                        [])))
               ))
            )))
         ],
       (EApplication ((EIdentifier (Id "is_even")), (EConstant (CInt 4)), [])))) |}]
;;

let%expect_test _ =
  parse_expression
    {| 
    let rec sequence_a n = if n = 0 then 1 else 2 * sequence_b (n - 1) 
    and sequence_b n = if n = 0 then 1 else 3 + sequence_a (n - 1) 
    in sequence_a 4
  |};
  [%expect
    {|
    (ERecLetIn (
       ((PVar (Id "sequence_a")),
        (EFun (((PVar (Id "n")), []),
           (EIfThenElse (
              (EApplication ((EIdentifier (Id "( = )")), (EIdentifier (Id "n")),
                 [(EConstant (CInt 0))])),
              (EConstant (CInt 1)),
              (Some (EApplication ((EIdentifier (Id "( * )")),
                       (EConstant (CInt 2)),
                       [(EApplication ((EIdentifier (Id "sequence_b")),
                           (EApplication ((EIdentifier (Id "( - )")),
                              (EIdentifier (Id "n")), [(EConstant (CInt 1))])),
                           []))
                         ]
                       )))
              ))
           ))),
       [((PVar (Id "sequence_b")),
         (EFun (((PVar (Id "n")), []),
            (EIfThenElse (
               (EApplication ((EIdentifier (Id "( = )")), (EIdentifier (Id "n")),
                  [(EConstant (CInt 0))])),
               (EConstant (CInt 1)),
               (Some (EApplication ((EIdentifier (Id "( + )")),
                        (EConstant (CInt 3)),
                        [(EApplication ((EIdentifier (Id "sequence_a")),
                            (EApplication ((EIdentifier (Id "( - )")),
                               (EIdentifier (Id "n")), [(EConstant (CInt 1))])),
                            []))
                          ]
                        )))
               ))
            )))
         ],
       (EApplication ((EIdentifier (Id "sequence_a")), (EConstant (CInt 4)), []))
       )) |}]
;;

let%expect_test _ =
  parse_expression
    {| 
    let rec length = function 
    | [] -> 0
    | _ :: xs -> 1 + length xs
    in length [1; 2; 3]
  |};
  [%expect
    {|
    (ERecLetIn (
       ((PVar (Id "length")),
        (EFunction ((PNill, (EConstant (CInt 0))),
           [((PListConstructor (PAny, (PVar (Id "xs")))),
             (EApplication ((EIdentifier (Id "( + )")), (EConstant (CInt 1)),
                [(EApplication ((EIdentifier (Id "length")),
                    (EIdentifier (Id "xs")), []))
                  ]
                )))
             ]
           ))),
       [],
       (EApplication ((EIdentifier (Id "length")),
          (EListConstructor ((EConstant (CInt 1)),
             (EListConstructor ((EConstant (CInt 2)),
                (EListConstructor ((EConstant (CInt 3)), EEmptyList))))
             )),
          []))
       )) |}]
;;

(* ---------------- *)

(* Function expression *)

let%expect_test _ =
  parse_expression {| 
    function (x, y) -> x + y
  |};
  [%expect
    {|
    (EFunction (
       ((PTuple ((PVar (Id "x")), (PVar (Id "y")), [])),
        (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
           [(EIdentifier (Id "y"))]))),
       [])) |}]
;;

let%expect_test _ =
  parse_expression {| 
    function 
    | [] -> "empty"
    | x :: xs -> "not empty"
  |};
  [%expect
    {|
    (EFunction ((PNill, (EConstant (CString "empty"))),
       [((PListConstructor ((PVar (Id "x")), (PVar (Id "xs")))),
         (EConstant (CString "not empty")))]
       )) |}]
;;

let%expect_test _ =
  parse_expression
    {| 
    function 
    | 0, y -> y
    | x, 0 -> x
    | x, y -> x + y
  |};
  [%expect
    {|
    (EFunction (
       ((PTuple ((PConst (CInt 0)), (PVar (Id "y")), [])), (EIdentifier (Id "y"))),
       [((PTuple ((PVar (Id "x")), (PConst (CInt 0)), [])),
         (EIdentifier (Id "x")));
         ((PTuple ((PVar (Id "x")), (PVar (Id "y")), [])),
          (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
             [(EIdentifier (Id "y"))])))
         ]
       )) |}]
;;

let%expect_test _ =
  parse_expression
    {| 
    function 
    | [] -> "empty"
    | [x] -> "one element"
    | x :: y :: xs -> "at least two elements"
  |};
  [%expect
    {|
    (EFunction ((PNill, (EConstant (CString "empty"))),
       [((PListConstructor ((PVar (Id "x")), PNill)),
         (EConstant (CString "one element")));
         ((PListConstructor ((PVar (Id "x")),
             (PListConstructor ((PVar (Id "y")), (PVar (Id "xs")))))),
          (EConstant (CString "at least two elements")))
         ]
       )) |}]
;;

(* ---------------- *)

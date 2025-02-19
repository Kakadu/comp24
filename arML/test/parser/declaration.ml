(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

(* Basic let ([PVar] pattern) *)

let%expect_test _ =
  parse_program {| let x = [] |};
  [%expect {|
    [(DOrdinary (((PVar (Id "x")), EEmptyList), []))] |}]
;;

let%expect_test _ =
  parse_program {| let x = 1 |};
  [%expect {|
    [(DOrdinary (((PVar (Id "x")), (EConstant (CInt 1))), []))] |}]
;;

let%expect_test _ =
  parse_program {| let f x = x |};
  [%expect
    {|
    [(DOrdinary (
        ((PVar (Id "f")), (EFun (((PVar (Id "x")), []), (EIdentifier (Id "x"))))),
        []))
      ] |}]
;;

let%expect_test _ =
  parse_program {| let f x y = x + y |};
  [%expect
    {|
    [(DOrdinary (
        ((PVar (Id "f")),
         (EFun (((PVar (Id "x")), [(PVar (Id "y"))]),
            (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
               [(EIdentifier (Id "y"))]))
            ))),
        []))
      ] |}]
;;

let%expect_test _ =
  parse_program {| 
   let f x y = x + y
   let main = f 1 2
   |};
  [%expect
    {|
    [(DOrdinary (
        ((PVar (Id "f")),
         (EFun (((PVar (Id "x")), [(PVar (Id "y"))]),
            (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
               [(EIdentifier (Id "y"))]))
            ))),
        []));
      (DOrdinary (
         ((PVar (Id "main")),
          (EApplication ((EIdentifier (Id "f")), (EConstant (CInt 1)),
             [(EConstant (CInt 2))]))),
         []))
      ] |}]
;;

let%expect_test _ =
  parse_program {| let f = let x = 1 in 2 + x * 3|};
  [%expect
    {|
    [(DOrdinary (
        ((PVar (Id "f")),
         (ELetIn (((PVar (Id "x")), (EConstant (CInt 1))), [],
            (EApplication ((EIdentifier (Id "( + )")), (EConstant (CInt 2)),
               [(EApplication ((EIdentifier (Id "( * )")),
                   (EIdentifier (Id "x")), [(EConstant (CInt 3))]))
                 ]
               ))
            ))),
        []))
      ] |}]
;;

(* ---------------- *)

(* Basic let (complex patterns) *)

let%expect_test _ =
  parse_program {| let _ = 1 |};
  [%expect {|
    [(DOrdinary ((PAny, (EConstant (CInt 1))), []))] |}]
;;

let%expect_test _ =
  parse_program {| let () = some_function some_argument |};
  [%expect
    {|
    [(DOrdinary (
        ((PConst CUnit),
         (EApplication ((EIdentifier (Id "some_function")),
            (EIdentifier (Id "some_argument")), []))),
        []))
      ] |}]
;;

let%expect_test _ =
  parse_program {| let (x, y) = (0, 0) |};
  [%expect
    {|
    [(DOrdinary (
        ((PTuple ((PVar (Id "x")), (PVar (Id "y")), [])),
         (ETuple ((EConstant (CInt 0)), (EConstant (CInt 0)), []))),
        []))
      ] |}]
;;

let%expect_test _ =
  parse_program {| let (x :: y) = x :: y |};
  [%expect
    {|
    [(DOrdinary (
        ((PListConstructor ((PVar (Id "x")), (PVar (Id "y")))),
         (EListConstructor ((EIdentifier (Id "x")), (EIdentifier (Id "y"))))),
        []))
      ] |}]
;;

let%expect_test _ =
  parse_program {| let [] = [] |};
  [%expect {|
    [(DOrdinary ((PNill, EEmptyList), []))] |}]
;;

let%expect_test _ =
  parse_program {| let ((x :: y, k :: l), (q :: j), o, _) = x :: y |};
  [%expect
    {|
    [(DOrdinary (
        ((PTuple (
            (PTuple ((PListConstructor ((PVar (Id "x")), (PVar (Id "y")))),
               (PListConstructor ((PVar (Id "k")), (PVar (Id "l")))), [])),
            (PListConstructor ((PVar (Id "q")), (PVar (Id "j")))),
            [(PVar (Id "o")); PAny])),
         (EListConstructor ((EIdentifier (Id "x")), (EIdentifier (Id "y"))))),
        []))
      ] |}]
;;

let%expect_test _ =
  parse_program {| 
   let f x y = x + y
   let (x, y) = (f 0 0, f 1 1)
   |};
  [%expect
    {|
    [(DOrdinary (
        ((PVar (Id "f")),
         (EFun (((PVar (Id "x")), [(PVar (Id "y"))]),
            (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
               [(EIdentifier (Id "y"))]))
            ))),
        []));
      (DOrdinary (
         ((PTuple ((PVar (Id "x")), (PVar (Id "y")), [])),
          (ETuple (
             (EApplication ((EIdentifier (Id "f")), (EConstant (CInt 0)),
                [(EConstant (CInt 0))])),
             (EApplication ((EIdentifier (Id "f")), (EConstant (CInt 1)),
                [(EConstant (CInt 1))])),
             []))),
         []))
      ] |}]
;;

let%expect_test _ =
  parse_program {| 
   let f x y = x + y
   let (x :: y) = (f 0 0 :: f 1 1)
   |};
  [%expect
    {|
    [(DOrdinary (
        ((PVar (Id "f")),
         (EFun (((PVar (Id "x")), [(PVar (Id "y"))]),
            (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "x")),
               [(EIdentifier (Id "y"))]))
            ))),
        []));
      (DOrdinary (
         ((PListConstructor ((PVar (Id "x")), (PVar (Id "y")))),
          (EListConstructor (
             (EApplication ((EIdentifier (Id "f")), (EConstant (CInt 0)),
                [(EConstant (CInt 0))])),
             (EApplication ((EIdentifier (Id "f")), (EConstant (CInt 1)),
                [(EConstant (CInt 1))]))
             ))),
         []))
      ] |}]
;;

(* ---------------- *)

(* Basic let with "and" ([PVar] pattern)] *)

let%expect_test _ =
  parse_program {| let x = 1 and y = 2 |};
  [%expect
    {|
    [(DOrdinary (((PVar (Id "x")), (EConstant (CInt 1))),
        [((PVar (Id "y")), (EConstant (CInt 2)))]))
      ] |}]
;;

let%expect_test _ =
  parse_program {| let f x = x and y = 2 |};
  [%expect
    {|
    [(DOrdinary (
        ((PVar (Id "f")), (EFun (((PVar (Id "x")), []), (EIdentifier (Id "x"))))),
        [((PVar (Id "y")), (EConstant (CInt 2)))]))
      ] |}]
;;

let%expect_test _ =
  parse_program {| let f x = x and g y = y |};
  [%expect
    {|
    [(DOrdinary (
        ((PVar (Id "f")), (EFun (((PVar (Id "x")), []), (EIdentifier (Id "x"))))),
        [((PVar (Id "g")), (EFun (((PVar (Id "y")), []), (EIdentifier (Id "y")))))
          ]
        ))
      ] |}]
;;

let%expect_test _ =
  parse_program {| let f x = x and g y = y and a b c = b + c |};
  [%expect
    {|
    [(DOrdinary (
        ((PVar (Id "f")), (EFun (((PVar (Id "x")), []), (EIdentifier (Id "x"))))),
        [((PVar (Id "g")), (EFun (((PVar (Id "y")), []), (EIdentifier (Id "y")))));
          ((PVar (Id "a")),
           (EFun (((PVar (Id "b")), [(PVar (Id "c"))]),
              (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "b")),
                 [(EIdentifier (Id "c"))]))
              )))
          ]
        ))
      ] |}]
;;

let%expect_test _ =
  parse_program
    {| 
   let f x = x 
   and g y = y 
   and a b c = b + c 

   let q y = y

   let k i = f q i
   |};
  [%expect
    {|
    [(DOrdinary (
        ((PVar (Id "f")), (EFun (((PVar (Id "x")), []), (EIdentifier (Id "x"))))),
        [((PVar (Id "g")), (EFun (((PVar (Id "y")), []), (EIdentifier (Id "y")))));
          ((PVar (Id "a")),
           (EFun (((PVar (Id "b")), [(PVar (Id "c"))]),
              (EApplication ((EIdentifier (Id "( + )")), (EIdentifier (Id "b")),
                 [(EIdentifier (Id "c"))]))
              )))
          ]
        ));
      (DOrdinary (
         ((PVar (Id "q")), (EFun (((PVar (Id "y")), []), (EIdentifier (Id "y"))))),
         []));
      (DOrdinary (
         ((PVar (Id "k")),
          (EFun (((PVar (Id "i")), []),
             (EApplication ((EIdentifier (Id "f")), (EIdentifier (Id "q")),
                [(EIdentifier (Id "i"))]))
             ))),
         []))
      ] |}]
;;

(* ---------------- *)

(* Basic let with "and" (complex patterns)] *)

let%expect_test _ =
  parse_program {| let (x, y) = (1, 2) and (e :: l) = (1 :: 2) |};
  [%expect
    {|
    [(DOrdinary (
        ((PTuple ((PVar (Id "x")), (PVar (Id "y")), [])),
         (ETuple ((EConstant (CInt 1)), (EConstant (CInt 2)), []))),
        [((PListConstructor ((PVar (Id "e")), (PVar (Id "l")))),
          (EListConstructor ((EConstant (CInt 1)), (EConstant (CInt 2)))))]
        ))
      ] |}]
;;

let%expect_test _ =
  parse_program {| let f x = x and (x, y) = (1, 2) |};
  [%expect
    {|
    [(DOrdinary (
        ((PVar (Id "f")), (EFun (((PVar (Id "x")), []), (EIdentifier (Id "x"))))),
        [((PTuple ((PVar (Id "x")), (PVar (Id "y")), [])),
          (ETuple ((EConstant (CInt 1)), (EConstant (CInt 2)), [])))]
        ))
      ] |}]
;;

let%expect_test _ =
  parse_program
    {| let f x = x and () = some_function some_args and ((x :: y), (z :: w)) = (1 :: 1, 1 :: 1) |};
  [%expect
    {|
    [(DOrdinary (
        ((PVar (Id "f")), (EFun (((PVar (Id "x")), []), (EIdentifier (Id "x"))))),
        [((PConst CUnit),
          (EApplication ((EIdentifier (Id "some_function")),
             (EIdentifier (Id "some_args")), [])));
          ((PTuple ((PListConstructor ((PVar (Id "x")), (PVar (Id "y")))),
              (PListConstructor ((PVar (Id "z")), (PVar (Id "w")))), [])),
           (ETuple (
              (EListConstructor ((EConstant (CInt 1)), (EConstant (CInt 1)))),
              (EListConstructor ((EConstant (CInt 1)), (EConstant (CInt 1)))),
              [])))
          ]
        ))
      ] |}]
;;

(* ---------------- *)

(* Recursive let *)

let%expect_test _ =
  parse_program {| let rec f x = f (x - 1) |};
  [%expect
    {|
    [(DRecursive (
        ((PVar (Id "f")),
         (EFun (((PVar (Id "x")), []),
            (EApplication ((EIdentifier (Id "f")),
               (EApplication ((EIdentifier (Id "( - )")), (EIdentifier (Id "x")),
                  [(EConstant (CInt 1))])),
               []))
            ))),
        []))
      ] |}]
;;

let%expect_test _ =
  parse_program {| 
   let rec f x = f (x - 1) 
   let rec g y = f (g y)
   |};
  [%expect
    {|
    [(DRecursive (
        ((PVar (Id "f")),
         (EFun (((PVar (Id "x")), []),
            (EApplication ((EIdentifier (Id "f")),
               (EApplication ((EIdentifier (Id "( - )")), (EIdentifier (Id "x")),
                  [(EConstant (CInt 1))])),
               []))
            ))),
        []));
      (DRecursive (
         ((PVar (Id "g")),
          (EFun (((PVar (Id "y")), []),
             (EApplication ((EIdentifier (Id "f")),
                (EApplication ((EIdentifier (Id "g")), (EIdentifier (Id "y")),
                   [])),
                []))
             ))),
         []))
      ] |}]
;;

let%expect_test _ =
  parse_program {| let rec f x = if x = 1 then x else x * f (x - 1) |};
  [%expect
    {|
    [(DRecursive (
        ((PVar (Id "f")),
         (EFun (((PVar (Id "x")), []),
            (EIfThenElse (
               (EApplication ((EIdentifier (Id "( = )")), (EIdentifier (Id "x")),
                  [(EConstant (CInt 1))])),
               (EIdentifier (Id "x")),
               (Some (EApplication ((EIdentifier (Id "( * )")),
                        (EIdentifier (Id "x")),
                        [(EApplication ((EIdentifier (Id "f")),
                            (EApplication ((EIdentifier (Id "( - )")),
                               (EIdentifier (Id "x")), [(EConstant (CInt 1))])),
                            []))
                          ]
                        )))
               ))
            ))),
        []))
      ] |}]
;;

(* ---------------- *)

(* Let with mutual recursion *)

let%expect_test _ =
  parse_program {| let rec f x = g x and g x = f x |};
  [%expect
    {|
    [(DRecursive (
        ((PVar (Id "f")),
         (EFun (((PVar (Id "x")), []),
            (EApplication ((EIdentifier (Id "g")), (EIdentifier (Id "x")), []))))),
        [((PVar (Id "g")),
          (EFun (((PVar (Id "x")), []),
             (EApplication ((EIdentifier (Id "f")), (EIdentifier (Id "x")), []))
             )))
          ]
        ))
      ] |}]
;;

let%expect_test _ =
  parse_program {| let rec f x = fun y -> f x y and g x = f x x |};
  [%expect
    {|
    [(DRecursive (
        ((PVar (Id "f")),
         (EFun (((PVar (Id "x")), []),
            (EFun (((PVar (Id "y")), []),
               (EApplication ((EIdentifier (Id "f")), (EIdentifier (Id "x")),
                  [(EIdentifier (Id "y"))]))
               ))
            ))),
        [((PVar (Id "g")),
          (EFun (((PVar (Id "x")), []),
             (EApplication ((EIdentifier (Id "f")), (EIdentifier (Id "x")),
                [(EIdentifier (Id "x"))]))
             )))
          ]
        ))
      ] |}]
;;

let%expect_test _ =
  parse_program
    {| 
      let rec f x = fun y -> f x y 
      and g x = f x x
      let rec q y x = q (y - 1) (x + 1)
   |};
  [%expect
    {|
    [(DRecursive (
        ((PVar (Id "f")),
         (EFun (((PVar (Id "x")), []),
            (EFun (((PVar (Id "y")), []),
               (EApplication ((EIdentifier (Id "f")), (EIdentifier (Id "x")),
                  [(EIdentifier (Id "y"))]))
               ))
            ))),
        [((PVar (Id "g")),
          (EFun (((PVar (Id "x")), []),
             (EApplication ((EIdentifier (Id "f")), (EIdentifier (Id "x")),
                [(EIdentifier (Id "x"))]))
             )))
          ]
        ));
      (DRecursive (
         ((PVar (Id "q")),
          (EFun (((PVar (Id "y")), [(PVar (Id "x"))]),
             (EApplication ((EIdentifier (Id "q")),
                (EApplication ((EIdentifier (Id "( - )")),
                   (EIdentifier (Id "y")), [(EConstant (CInt 1))])),
                [(EApplication ((EIdentifier (Id "( + )")),
                    (EIdentifier (Id "x")), [(EConstant (CInt 1))]))
                  ]
                ))
             ))),
         []))
      ] |}]
;;

(* ---------------- *)

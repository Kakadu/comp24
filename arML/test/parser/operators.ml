(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

(* Binary operations *)

let%expect_test _ =
  parse_expression {| 1 + 1 |};
  [%expect
    {|
    (EApplication ((EIdentifier (Id "( + )")), (EConstant (CInt 1)),
       [(EConstant (CInt 1))])) |}]
;;

let%expect_test _ =
  parse_expression {| 1 + 2 * 3 |};
  [%expect
    {|
    (EApplication ((EIdentifier (Id "( + )")), (EConstant (CInt 1)),
       [(EApplication ((EIdentifier (Id "( * )")), (EConstant (CInt 2)),
           [(EConstant (CInt 3))]))
         ]
       )) |}]
;;

let%expect_test _ =
  parse_expression {| 1 + 2 / 3 |};
  [%expect
    {|
    (EApplication ((EIdentifier (Id "( + )")), (EConstant (CInt 1)),
       [(EApplication ((EIdentifier (Id "( / )")), (EConstant (CInt 2)),
           [(EConstant (CInt 3))]))
         ]
       )) |}]
;;

let%expect_test _ =
  parse_expression {| (1 + 2) * (3 + 4) / (1 + (2 * 3)) |};
  [%expect
    {|
    (EApplication ((EIdentifier (Id "( / )")),
       (EApplication ((EIdentifier (Id "( * )")),
          (EApplication ((EIdentifier (Id "( + )")), (EConstant (CInt 1)),
             [(EConstant (CInt 2))])),
          [(EApplication ((EIdentifier (Id "( + )")), (EConstant (CInt 3)),
              [(EConstant (CInt 4))]))
            ]
          )),
       [(EApplication ((EIdentifier (Id "( + )")), (EConstant (CInt 1)),
           [(EApplication ((EIdentifier (Id "( * )")), (EConstant (CInt 2)),
               [(EConstant (CInt 3))]))
             ]
           ))
         ]
       )) |}]
;;

let%expect_test _ =
  parse_expression {| 1 < 2 && 2 > 1 |};
  [%expect
    {|
    (EApplication ((EIdentifier (Id "( && )")),
       (EApplication ((EIdentifier (Id "( < )")), (EConstant (CInt 1)),
          [(EConstant (CInt 2))])),
       [(EApplication ((EIdentifier (Id "( > )")), (EConstant (CInt 2)),
           [(EConstant (CInt 1))]))
         ]
       )) |}]
;;

let%expect_test _ =
  parse_expression {| 1 <= 2 || 3 >= 2 |};
  [%expect
    {|
    (EApplication ((EIdentifier (Id "( || )")),
       (EApplication ((EIdentifier (Id "( <= )")), (EConstant (CInt 1)),
          [(EConstant (CInt 2))])),
       [(EApplication ((EIdentifier (Id "( >= )")), (EConstant (CInt 3)),
           [(EConstant (CInt 2))]))
         ]
       )) |}]
;;

let%expect_test _ =
  parse_expression {| 1 <> 2 && 3 <> 4 || 1 = 1|};
  [%expect
    {|
    (EApplication ((EIdentifier (Id "( || )")),
       (EApplication ((EIdentifier (Id "( && )")),
          (EApplication ((EIdentifier (Id "( <> )")), (EConstant (CInt 1)),
             [(EConstant (CInt 2))])),
          [(EApplication ((EIdentifier (Id "( <> )")), (EConstant (CInt 3)),
              [(EConstant (CInt 4))]))
            ]
          )),
       [(EApplication ((EIdentifier (Id "( = )")), (EConstant (CInt 1)),
           [(EConstant (CInt 1))]))
         ]
       )) |}]
;;

let%expect_test _ =
  parse_expression {| (1 * 2 + 3 / 3) >= (3 / 3 / 3) || (1 <> 9 / 3) |};
  [%expect
    {|
    (EApplication ((EIdentifier (Id "( || )")),
       (EApplication ((EIdentifier (Id "( >= )")),
          (EApplication ((EIdentifier (Id "( + )")),
             (EApplication ((EIdentifier (Id "( * )")), (EConstant (CInt 1)),
                [(EConstant (CInt 2))])),
             [(EApplication ((EIdentifier (Id "( / )")), (EConstant (CInt 3)),
                 [(EConstant (CInt 3))]))
               ]
             )),
          [(EApplication ((EIdentifier (Id "( / )")),
              (EApplication ((EIdentifier (Id "( / )")), (EConstant (CInt 3)),
                 [(EConstant (CInt 3))])),
              [(EConstant (CInt 3))]))
            ]
          )),
       [(EApplication ((EIdentifier (Id "( <> )")), (EConstant (CInt 1)),
           [(EApplication ((EIdentifier (Id "( / )")), (EConstant (CInt 9)),
               [(EConstant (CInt 3))]))
             ]
           ))
         ]
       )) |}]
;;

(* ---------------- *)

(* Unary operartions *)

let%expect_test _ =
  parse_expression {| -1 |};
  [%expect
    {|
    (EApplication ((EIdentifier (Id "U-")), (EConstant (CInt 1)), [])) |}]
;;

let%expect_test _ =
  parse_expression {| +1 |};
  [%expect
    {|
    (EApplication ((EIdentifier (Id "U+")), (EConstant (CInt 1)), [])) |}]
;;

let%expect_test _ =
  parse_expression {| not true |};
  [%expect
    {|
    (EApplication ((EIdentifier (Id "UNot")), (EConstant (CBool true)), [])) |}]
;;

let%expect_test _ =
  parse_expression {| f (-1) |};
  [%expect
    {|
    (EApplication ((EIdentifier (Id "f")),
       (EApplication ((EIdentifier (Id "U-")), (EConstant (CInt 1)), [])),
       [])) |}]
;;

let%expect_test _ =
  parse_expression {| fun f x -> - (f (-x)) |};
  [%expect
    {|
    (EFun (((PVar (Id "f")), [(PVar (Id "x"))]),
       (EApplication ((EIdentifier (Id "U-")),
          (EApplication ((EIdentifier (Id "f")),
             (EApplication ((EIdentifier (Id "U-")), (EIdentifier (Id "x")), [])),
             [])),
          []))
       )) |}]
;;

let%expect_test _ =
  parse_expression {| not (1 + (-2) * (+3) >= 3) |};
  [%expect
    {|
    (EApplication ((EIdentifier (Id "UNot")),
       (EApplication ((EIdentifier (Id "( >= )")),
          (EApplication ((EIdentifier (Id "( + )")), (EConstant (CInt 1)),
             [(EApplication ((EIdentifier (Id "( * )")),
                 (EApplication ((EIdentifier (Id "U-")), (EConstant (CInt 2)),
                    [])),
                 [(EApplication ((EIdentifier (Id "U+")), (EConstant (CInt 3)),
                     []))
                   ]
                 ))
               ]
             )),
          [(EConstant (CInt 3))])),
       [])) |}]
;;

(* ---------------- *)

open Lexing
open Lexer
open Base
open Ast

(* Prints the line number and character number where the error occurred.*)
let error_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Format.asprintf "Line %d Pos %d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
;;

let parse_program ?(print_ast = false) (code : string)
  : (Ast.definition list, string) result
  =
  let lexbuf = Lexing.from_string code in
  try
    let ast = Menhir.program Lexer.token lexbuf in
    if print_ast then List.iter ast ~f:(fun d -> print_endline (show_definition d));
    Ok ast
  with
  | SyntaxError msg ->
    let error_msg = Format.sprintf "%s: %s@." (error_position lexbuf) msg in
    Error error_msg
  | Menhir.Error ->
    let error_msg =
      Format.sprintf
        "%s: syntax error\nlex_buffer:\n%s"
        (error_position lexbuf)
        (Bytes.to_string lexbuf.lex_buffer)
    in
    Error error_msg
;;

let test code =
  match parse_program code ~print_ast:true with
  | Ok _ -> ()
  | Error e -> print_endline e
;;

let%expect_test _ =
  test "let _ = 12";
  [%expect {| (DLet (NonRec, PWild, (EConst (CInt 12)))) |}]
;;

let%expect_test _ =
  test "let _ = -12";
  [%expect {| (DLet (NonRec, PWild, (EConst (CInt -12)))) |}]
;;

let%expect_test _ =
  test "let _ = (12)";
  [%expect {| (DLet (NonRec, PWild, (EConst (CInt 12)))) |}]
;;

let%expect_test _ =
  test "let _ = (-12)";
  [%expect {| (DLet (NonRec, PWild, (EConst (CInt -12)))) |}]
;;

let%expect_test _ =
  test "let _ = 1+2";
  [%expect
    {|
      (DLet (NonRec, PWild,
         (EApp ((EApp ((EVar "( + )"), (EConst (CInt 1)))), (EConst (CInt 2)))))) |}]
;;

let%expect_test _ =
  test "let _ = 1 + -2";
  [%expect
    {|
    (DLet (NonRec, PWild,
       (EApp ((EApp ((EVar "( + )"), (EConst (CInt 1)))), (EConst (CInt -2)))))) |}]
;;

let%expect_test _ =
  test "let _ = (-1 - -3) * (-2 + 4 / 2)";
  [%expect
    {|
    (DLet (NonRec, PWild,
       (EApp (
          (EApp ((EVar "( * )"),
             (EApp ((EApp ((EVar "( - )"), (EConst (CInt -1)))),
                (EConst (CInt -3))))
             )),
          (EApp ((EApp ((EVar "( + )"), (EConst (CInt -2)))),
             (EApp ((EApp ((EVar "( / )"), (EConst (CInt 4)))), (EConst (CInt 2))
                ))
             ))
          ))
       )) |}]
;;

let%expect_test _ =
  test "let x = 42";
  [%expect {| (DLet (NonRec, (PIdent ("x", None)), (EConst (CInt 42)))) |}]
;;

let%expect_test _ =
  test "let _ = let x = 42 in x";
  [%expect
    {|
    (DLet (NonRec, PWild,
       (ELetIn ((DLet (NonRec, (PIdent ("x", None)), (EConst (CInt 42)))),
          (EVar "x")))
       )) |}]
;;

let%expect_test _ =
  test "let _ = ()";
  [%expect {| (DLet (NonRec, PWild, (EConst CUnit))) |}]
;;

let%expect_test _ =
  test "let id = fun x -> x";
  [%expect
    {|
    (DLet (NonRec, (PIdent ("id", None)),
       (EFun ((PIdent ("x", None)), (EVar "x"))))) |}]
;;

let%expect_test _ =
  test "let _ = fun x -> somefunc x";
  [%expect
    {|
      (DLet (NonRec, PWild,
         (EFun ((PIdent ("x", None)), (EApp ((EVar "somefunc"), (EVar "x"))))))) |}]
;;

let%expect_test _ =
  test "let max = fun x -> fun y -> if x < y then y else x";
  [%expect
    {|
    (DLet (NonRec, (PIdent ("max", None)),
       (EFun ((PIdent ("x", None)),
          (EFun ((PIdent ("y", None)),
             (EIfElse ((EApp ((EApp ((EVar "( < )"), (EVar "x"))), (EVar "y"))),
                (EVar "y"), (EVar "x")))
             ))
          ))
       )) |}]
;;

let%expect_test _ =
  test "let rec factorial = fun x -> if x > 1 then x * (factorial (x-1)) else 1";
  [%expect
    {|
      (DLet (Rec, (PIdent ("factorial", None)),
         (EFun ((PIdent ("x", None)),
            (EIfElse (
               (EApp ((EApp ((EVar "( > )"), (EVar "x"))), (EConst (CInt 1)))),
               (EApp ((EApp ((EVar "( * )"), (EVar "x"))),
                  (EApp ((EVar "factorial"),
                     (EApp ((EVar "x"), (EConst (CInt -1))))))
                  )),
               (EConst (CInt 1))))
            ))
         )) |}]
;;

let%expect_test _ =
  test
    "let rec factorial = fun x -> fun cont -> if x > 1 then factorial (n - 1) (fun n -> \
     cont (x * n)) else cont 1 ";
  [%expect
    {|
    (DLet (Rec, (PIdent ("factorial", None)),
       (EFun ((PIdent ("x", None)),
          (EFun ((PIdent ("cont", None)),
             (EIfElse (
                (EApp ((EApp ((EVar "( > )"), (EVar "x"))), (EConst (CInt 1)))),
                (EApp (
                   (EApp ((EVar "factorial"),
                      (EApp ((EApp ((EVar "( - )"), (EVar "n"))),
                         (EConst (CInt 1))))
                      )),
                   (EFun ((PIdent ("n", None)),
                      (EApp ((EVar "cont"),
                         (EApp ((EApp ((EVar "( * )"), (EVar "x"))), (EVar "n")))
                         ))
                      ))
                   )),
                (EApp ((EVar "cont"), (EConst (CInt 1))))))
             ))
          ))
       )) |}]
;;

let%expect_test _ =
  test "let plus_one = fun x -> let one = 1 in x + one";
  [%expect
    {|
      (DLet (NonRec, (PIdent ("plus_one", None)),
         (EFun ((PIdent ("x", None)),
            (ELetIn ((DLet (NonRec, (PIdent ("one", None)), (EConst (CInt 1)))),
               (EApp ((EApp ((EVar "( + )"), (EVar "x"))), (EVar "one")))))
            ))
         )) |}]
;;

let%expect_test _ =
  test "let bool = true";
  [%expect {| (DLet (NonRec, (PIdent ("bool", None)), (EConst (CBool true)))) |}]
;;

let%expect_test _ =
  test "let one = 1 let two = 2";
  [%expect
    {|
    (DLet (NonRec, (PIdent ("one", None)), (EConst (CInt 1))))
    (DLet (NonRec, (PIdent ("two", None)), (EConst (CInt 2)))) |}]
;;

let%expect_test _ =
  test "let (x:int) = 42";
  [%expect {| (DLet (NonRec, (PIdent ("x", (Some TAInt))), (EConst (CInt 42)))) |}]
;;

let%expect_test _ =
  test "let (id:int->int) = fun (x:int) -> x";
  [%expect
    {|
    (DLet (NonRec, (PIdent ("id", (Some (TAFun (TAInt, TAInt))))),
       (EFun ((PIdent ("x", (Some TAInt))), (EVar "x"))))) |}]
;;

(* TODO: more tests for patterns and type annotations *)

let%expect_test _ =
  test "let _ = (1, true, ())";
  [%expect
    {|  
    (DLet (NonRec, PWild,
       (ETuple [(EConst (CInt 1)); (EConst (CBool true)); (EConst CUnit)])))|}]
;;

let%expect_test _ =
  test
    {|
    let add = fun x -> fun y -> x + y
    let add_one = add 1
    let x = add_one 2
  |};
  [%expect
    {|
    (DLet (NonRec, (PIdent ("add", None)),
       (EFun ((PIdent ("x", None)),
          (EFun ((PIdent ("y", None)),
             (EApp ((EApp ((EVar "( + )"), (EVar "x"))), (EVar "y")))))
          ))
       ))
    (DLet (NonRec, (PIdent ("add_one", None)),
       (EApp ((EVar "add"), (EConst (CInt 1))))))
    (DLet (NonRec, (PIdent ("x", None)),
       (EApp ((EVar "add_one"), (EConst (CInt 2))))))
  |}]
;;

let%expect_test _ =
  test "let _ = (1 + 2 * 3) / 4 - (5 * -6 + -7) / 8 * 9";
  [%expect
    {|
    (DLet (NonRec, PWild,
       (EApp (
          (EApp ((EVar "( - )"),
             (EApp (
                (EApp ((EVar "( / )"),
                   (EApp ((EApp ((EVar "( + )"), (EConst (CInt 1)))),
                      (EApp ((EApp ((EVar "( * )"), (EConst (CInt 2)))),
                         (EConst (CInt 3))))
                      ))
                   )),
                (EConst (CInt 4))))
             )),
          (EApp (
             (EApp ((EVar "( * )"),
                (EApp (
                   (EApp ((EVar "( / )"),
                      (EApp (
                         (EApp ((EVar "( + )"),
                            (EApp ((EApp ((EVar "( * )"), (EConst (CInt 5)))),
                               (EConst (CInt -6))))
                            )),
                         (EConst (CInt -7))))
                      )),
                   (EConst (CInt 8))))
                )),
             (EConst (CInt 9))))
          ))
       )) |}]
;;

let%expect_test _ =
  test {| let tuple = (true, 42, fun x -> x, (1, 2), if true then false else true) |};
  [%expect
    {|
    (DLet (NonRec, (PIdent ("tuple", None)),
       (ETuple
          [(EConst (CBool true)); (EConst (CInt 42));
            (EFun ((PIdent ("x", None)), (EVar "x")));
            (ETuple [(EConst (CInt 1)); (EConst (CInt 2))]);
            (EIfElse ((EConst (CBool true)), (EConst (CBool false)),
               (EConst (CBool true))))
            ])
       )) |}]
;;

let%expect_test _ =
  test
    {| 
  let rec fib = fun (n:int) -> match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> (fib (n - 1)) + (fib (n - 2))
  |};
  [%expect
    {|
    (DLet (Rec, (PIdent ("fib", None)),
       (EFun ((PIdent ("n", (Some TAInt))),
          (EMatch ((PIdent ("n", None)),
             [((PConst (CInt 0)), (EConst (CInt 0)));
               ((PConst (CInt 1)), (EConst (CInt 1)));
               (PWild,
                (EApp (
                   (EApp ((EVar "( + )"),
                      (EApp ((EVar "fib"),
                         (EApp ((EApp ((EVar "( - )"), (EVar "n"))),
                            (EConst (CInt 1))))
                         ))
                      )),
                   (EApp ((EVar "fib"),
                      (EApp ((EApp ((EVar "( - )"), (EVar "n"))),
                         (EConst (CInt 2))))
                      ))
                   )))
               ]
             ))
          ))
       ))
     |}]
;;

let%expect_test _ =
  test {| let (x, y) = (1, 2) |};
  [%expect
    {|
    (DLet (NonRec, (PTuple [(PIdent ("x", None)); (PIdent ("y", None))]),
       (ETuple [(EConst (CInt 1)); (EConst (CInt 2))]))) |}]
;;

let%expect_test _ =
  test {| 
  let (+) = fun (x:bool) -> fun (y:bool) -> x || y 
  let x = true + false
  |};
  [%expect
    {|
    (DLet (NonRec, (PIdent ("( + )", None)),
       (EFun ((PIdent ("x", (Some TABool))),
          (EFun ((PIdent ("y", (Some TABool))),
             (EApp ((EApp ((EVar "( || )"), (EVar "x"))), (EVar "y")))))
          ))
       ))
    (DLet (NonRec, (PIdent ("x", None)),
       (EApp ((EApp ((EVar "( + )"), (EConst (CBool true)))),
          (EConst (CBool false))))
       )) |}]
;;

let%expect_test _ =
  test {| let (x, y) = (not true, not false) |};
  [%expect {|
    (DLet (NonRec, (PTuple [(PIdent ("x", None)); (PIdent ("y", None))]),
       (ETuple [(EConst (CBool false)); (EConst (CBool true))])))
     |}]
;;

let%expect_test _ =
  test {| 
  let x = 1 
  let y = -x
  let z = true
  let w = not z
  |};
  [%expect {|
    (DLet (NonRec, (PIdent ("x", None)), (EConst (CInt 1))))
    (DLet (NonRec, (PIdent ("y", None)), (EApp ((EVar "[ - ]"), (EVar "x")))))
    (DLet (NonRec, (PIdent ("z", None)), (EConst (CBool true))))
    (DLet (NonRec, (PIdent ("w", None)), (EApp ((EVar "not"), (EVar "z"))))) |}]
;;

let%expect_test _ =
  test {| let x = (42) |};
  [%expect {| (DLet (NonRec, (PIdent ("x", None)), (EConst (CInt 42)))) |}]
;;

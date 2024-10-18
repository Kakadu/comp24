open Lexing
open Lexer
open Core
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
  [%expect {| (DLet (NonRec, PWild, (EUnaryOp (Neg, (EConst (CInt 12)))))) |}]
;;

let%expect_test _ =
  test "let _ = (12)";
  [%expect {| (DLet (NonRec, PWild, (EConst (CInt 12)))) |}]
;;

let%expect_test _ =
  test "let _ = (-12)";
  [%expect {| (DLet (NonRec, PWild, (EUnaryOp (Neg, (EConst (CInt 12)))))) |}]
;;

let%expect_test _ =
  test "let _ = 1+2";
  [%expect
    {|
      (DLet (NonRec, PWild, (EBinaryOp (Add, (EConst (CInt 1)), (EConst (CInt 2))))
         )) |}]
;;

let%expect_test _ =
  test "let _ = 1 + -2";
  [%expect
    {|
    (DLet (NonRec, PWild,
       (EBinaryOp (Add, (EConst (CInt 1)), (EUnaryOp (Neg, (EConst (CInt 2))))))
       )) |}]
;;

let%expect_test _ =
  test "let _ = (-1 - -3) * (-2 + 4 / 2)";
  [%expect
    {|
    (DLet (NonRec, PWild,
       (EBinaryOp (Mul,
          (EBinaryOp (Sub, (EUnaryOp (Neg, (EConst (CInt 1)))),
             (EUnaryOp (Neg, (EConst (CInt 3)))))),
          (EBinaryOp (Add, (EUnaryOp (Neg, (EConst (CInt 2)))),
             (EBinaryOp (Div, (EConst (CInt 4)), (EConst (CInt 2))))))
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
             (EIfElse ((EBinaryOp (Lt, (EVar "x"), (EVar "y"))), (EVar "y"),
                (EVar "x")))
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
            (EIfElse ((EBinaryOp (Gt, (EVar "x"), (EConst (CInt 1)))),
               (EBinaryOp (Mul, (EVar "x"),
                  (EApp ((EVar "factorial"),
                     (EBinaryOp (Sub, (EVar "x"), (EConst (CInt 1))))))
                  )),
               (EConst (CInt 1))))
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
               (EBinaryOp (Add, (EVar "x"), (EVar "one")))))
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

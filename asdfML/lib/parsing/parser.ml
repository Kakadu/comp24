(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Lexing
open Lexer
open Base

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
    if print_ast
    then List.iter ast ~f:(fun d -> Pp_ast.pp_definition Format.std_formatter d);
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
  test "let _ = -12";
  test "let _ = (12)";
  test "let _ = (-12)";
  [%expect {|
    let _ = 12
    let _ = -12
    let _ = 12
    let _ = -12 |}]
;;

let%expect_test _ =
  test "let _ = 1+2";
  test "let _ = 1 + -2";
  test "let _ = (-1 - -3) * (-2 + 4 / 2)";
  [%expect
    {|
      let _ = (( + ) 1 2)
      let _ = (( + ) 1 -2)
      let _ = (( * ) (( - ) -1 -3) (( + ) -2 (( / ) 4 2))) |}]
;;

let%expect_test _ =
  test "let _ = let x = 42 in x";
  [%expect {|
    let _ = let x = 42
     in x |}]
;;

let%expect_test _ =
  test "let _ = ()";
  [%expect {| let _ = () |}]
;;

let%expect_test _ =
  test "let id = fun x -> x";
  [%expect {|
    let id = fun x -> x |}]
;;

let%expect_test _ =
  test "let _ = fun x -> somefunc x";
  [%expect {|
      let _ = fun x -> (somefunc x) |}]
;;

let%expect_test _ =
  test "let max = fun x -> fun y -> if x < y then y else x";
  [%expect {|
    let max = fun x -> fun y -> if (( < ) x y) then y else x |}]
;;

let%expect_test _ =
  test "let rec factorial = fun x -> if x > 1 then x * (factorial (x - 1)) else 1";
  [%expect
    {|
      let rec factorial = fun x -> if (( > ) x 1) then (( * ) x (factorial (( - ) x 1))) else 1 |}]
;;

let%expect_test _ =
  test
    "let rec factorial = fun x -> fun cont -> if x > 1 then factorial (n - 1) (fun n -> \
     cont (x * n)) else cont 1 ";
  [%expect
    {|
    let rec factorial = fun x -> fun cont -> if (( > ) x 1) then (factorial (( - ) n 1) fun n -> (cont (( * ) x n))) else (cont 1) |}]
;;

let%expect_test _ =
  test "let plus_one = fun x -> let one = 1 in x + one";
  [%expect {|
      let plus_one = fun x -> let one = 1
       in (( + ) x one) |}]
;;

let%expect_test _ =
  test "let bool = true";
  [%expect {| let bool = true |}]
;;

let%expect_test _ =
  test "let one = 1 let two = 2";
  [%expect {|
    let one = 1
    let two = 2 |}]
;;

let%expect_test _ =
  test "let (x: int) = 42";
  [%expect {| let (x: int) = 42 |}]
;;

let%expect_test _ =
  test "let (id: int->int) = fun (x: int) -> x";
  [%expect {|
    let (id: int -> int) = fun (x: int) -> x |}]
;;

(* TODO: more tests for patterns and type annotations *)

let%expect_test _ =
  test "let _ = (1, true, ())";
  [%expect {|  
    let _ = (1, true, ())|}]
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
    let add = fun x -> fun y -> (( + ) x y)
    let add_one = (add 1)
    let x = (add_one 2)
  |}]
;;

let%expect_test _ =
  test "let _ = (1 + 2 * 3) / 4 - (5 * -6 + -7) / 8 * 9";
  [%expect
    {|
    let _ = (( - ) (( / ) (( + ) 1 (( * ) 2 3)) 4) (( * ) (( / ) (( + ) (( * ) 5 -6) -7) 8) 9)) |}]
;;

let%expect_test _ =
  test {| let tuple = (true, 42, fun x -> x, (1, 2), if true then false else true) |};
  [%expect
    {|
    let tuple = (true, 42, fun x -> x, (1, 2), if true then false else true) |}]
;;

let%expect_test _ =
  test
    {| 
  let rec fib = fun (n: int) -> match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> (fib (n - 1)) + (fib (n - 2))
  |};
  [%expect
    {|
    let rec fib = fun (n: int) -> match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> (( + ) (fib (( - ) n 1)) (fib (( - ) n 2)))
     |}]
;;

let%expect_test _ =
  test {| let (x, y) = (1, 2) |};
  [%expect {|
    let (x, y) = (1, 2) |}]
;;

let%expect_test _ =
  test {| 
  let (+) = fun (x: bool) -> fun (y: bool) -> x || y 
  let x = true + false
  |};
  [%expect
    {|
    let ( + ) = fun (x: bool) -> fun (y: bool) -> (( || ) x y)
    let x = (( + ) true false) |}]
;;

let%expect_test _ =
  test {| let (x, y) = (not true, not false) |};
  [%expect {|
    let (x, y) = (false, true)
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
    let x = 1
    let y = ([ - ] x)
    let z = true
    let w = (not z) |}]
;;

let%expect_test _ =
  test {| let x = (42) |};
  [%expect {| let x = 42 |}]
;;

let%expect_test _ =
  test
    {| 
  let [x; y; z] = [1; 2; 3] 
  let w = true :: [not true]
  let _ = match w with
  | [true; false] -> true
  | _ -> false
  |};
  [%expect
    {|
    let [x; y; z] = [1; 2; 3]
    let w = (( :: ) true [false])
    let _ = match w with
    | [true; false] -> true
    | _ -> false |}]
;;

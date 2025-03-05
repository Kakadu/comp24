(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ParsingTests = struct
  let parse_test s =
    match Parser.parse_program s with
    | Ok actual -> Format.printf "%a\n" AstLib.Pp_ast.pp_prog actual
    | Error err -> Format.printf "%s\n" err
  ;;

  let%expect_test "Simple constant declaration" =
    parse_test {| let a = 3 |};
    [%expect {| let a = 3 |}]
  ;;

  let%expect_test "Typed constant declaration" =
    parse_test {| let a : int = (3 : int) |};
    [%expect {| let (a : int) = (3 : int) |}]
  ;;

  let%expect_test "Parses with expected operator precedence 1" =
    parse_test {| let a : int = 2 ++ 2 ** 2 |};
    [%expect {| let (a : int) = (2 ++ (2 ** 2)) |}]
  ;;

  let%expect_test "Parses with expected operator precedence 2" =
    parse_test {| let a = (2 ++ 2) ** 2 |};
    [%expect {| let a = ((2 ++ 2) ** 2) |}]
  ;;

  let%expect_test "Let with nested let" =
    parse_test {| let a = let f x = x in 13 |};
    [%expect {| 
      let a = let f = (fun x -> x)
      in 13
      |}]
  ;;

  let%expect_test "Custom operator definition: simple" =
    parse_test {| let ( += ) = x -$ 1 |};
    [%expect {| let ( += ) = (x -$ 1) |}]
  ;;

  let%expect_test "Mutual recursion with custom operators" =
    parse_test {|
    let rec ( += ) x = x -$ 1 
    and (-$) = fun x y -> ( + ) x y;;
 |};
    [%expect
      {| 
      let rec ( += ) = (fun x -> (x -$ 1))
      and ( -$ ) = (fun x -> (fun y -> (x + y)))
      |}]
  ;;

  let%expect_test "Parenthesized operator" =
    parse_test {| let a = (+) x y |};
    [%expect {| let a = (x + y) |}]
  ;;

  let%expect_test "Custom infix operator definition" =
    parse_test {| let ( += ) a b = (~-) 1 |};
    [%expect {| let ( += ) = (fun a -> (fun b -> (( ~- ) 1))) |}]
  ;;

  let%expect_test "Function with typed argument: function type" =
    parse_test {| let f (lst: (int -> int)) = g |};
    [%expect {| let f = (fun (lst : int -> int) -> g) |}]
  ;;

  let%expect_test "Double nested let" =
    parse_test {| let a = let f x = x + 1 in let b = 5 in f (b + 1) |};
    [%expect
      {| 
      let a = let f = (fun x -> (x + 1))
      in let b = 5
      in f (b + 1)
    |}]
  ;;

  let%expect_test "Complex expression with list and tuple" =
    parse_test "let a =([3;3], [4;4]) :: [([5;5],[6;6]);([7;7],[8;8])]";
    [%expect
      {| let a = ((3 :: 3 :: []), (4 :: 4 :: [])) :: ((5 :: 5 :: []), (6 :: 6 :: [])) :: ((7 :: 7 :: []), (8 :: 8 :: [])) :: [] |}]
  ;;

  let%expect_test "Match tuple: simple pattern matching" =
    parse_test
      {|
    let a = 
      match l with 
      | hd, tl -> hd 
      | _ -> 0
    |};
    [%expect {|
      let a = match l with
      | (hd, tl) -> hd
      | _ -> 0|}]
  ;;

  let%expect_test "Function with typed argument: polymorphic list" =
    parse_test {| let f (lst: ('b list)) = lst |};
    [%expect {| let f = (fun (lst : 'b list) -> lst) |}]
  ;;

  let%expect_test "Unary minus parsing" =
    parse_test "let a = - 3";
    [%expect {| let a = (- 3) |}]
  ;;

  let%expect_test "Syntax error: unexpected arrow" =
    parse_test "let a = 3 -> 3";
    [%expect {| Syntax error |}]
  ;;

  let%expect_test "Operator precedence with function application" =
    parse_test "let a = f a + f b";
    [%expect {| let a = ((f a) + (f b)) |}]
  ;;

  let%expect_test "Operator precedence with unary minus and function application" =
    parse_test "let a = - f 3 - f 4";
    [%expect {| let a = ((- (f 3)) - (f 4)) |}]
  ;;

  let%expect_test "List pattern matching in function argument" =
    parse_test "let f (hd::tl) = 3";
    [%expect {| let f = (fun hd :: tl -> 3) |}]
  ;;

  let%expect_test "Tuple pattern matching in function argument" =
    parse_test "let f (x,y) = x + y";
    [%expect {| let f = (fun (x, y) -> (x + y)) |}]
  ;;

  let%expect_test "Tuple pattern matching with typed element" =
    parse_test "let f ((x: 'a),y) =  x * y";
    [%expect {| let f = (fun ((x : 'a), y) -> (x * y)) |}]
  ;;

  let%expect_test "List pattern matching with nested tuple pattern" =
    parse_test "let f (x :: (x, y)) = 3";
    [%expect {| let f = (fun x :: (x, y) -> 3) |}]
  ;;

  let%expect_test "Complex mutual recursion with typed let binding" =
    parse_test
      {| 
      let blst : int=
        let rec f x = if x > 0 then g (x - 1) else 1
        and g x = if x > 0 then f (x - 1) + g (x - 2) else 1 in
        f 10
      |};
    [%expect
      {|
      let (blst : int) = let rec f = (fun x -> if (x > 0) then g (x - 1) else 1)
      and g = (fun x -> if (x > 0) then (f (x - 1) + g (x - 2)) else 1)
      in (f 10) |}]
  ;;

  let%expect_test "Match tuple with boolean values and typed arguments" =
    parse_test
      {| let xor (x : bool) (y : bool) = 
      match (x, y) with 
      | (true, true) -> false 
      | (true, false) -> true 
      | (false, true) -> true
      | (false, false) -> false
      |};
    [%expect
      {|
      let xor = (fun (x : bool) -> (fun (y : bool) -> match (x, y) with
      | (true, true) -> false
      | (true, false) -> true
      | (false, true) -> true
      | (false, false) -> false)) |}]
  ;;

  let%expect_test "Deeply nested function application" =
    parse_test "let a = let f x = x in f (f (f (5 + f 5+5+5+f 5) + 5) + 5) + 5";
    [%expect
      {|
      let a = let f = (fun x -> x)
      in (f (f (f ((((5 + (f 5)) + 5) + 5) + (f 5)) + 5) + 5) + 5) |}]
  ;;

  let%expect_test "Type annotation on function application result" =
    parse_test "let a = f x : int";
    [%expect {|
      let a = ((f x) : int)
      |}]
  ;;

  let%expect_test "Double semicolon separator" =
    parse_test {|
    let a = 42;;
    let b = 42;;
    |};
    [%expect {|
      let a = 42;;
      let b = 42
      |}]
  ;;

  let%expect_test "Empty list pattern" =
    parse_test {|
      let map [] = []
    |};
    [%expect {|
      let map = (fun [] -> [])
      |}]
  ;;
end

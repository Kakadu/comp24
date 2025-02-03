(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module ParsingTests = struct
  let parse_test s =
    match Parser.parse_program s with
    | Ok actual -> Format.printf "%a\n" AstLib.Pp_ast.pp_prog actual
    | Error err -> Format.printf "%s\n" err
  ;;

  let%expect_test _ =
    parse_test {| let a = 3 |};
    [%expect {| let a = 3 |}]
  ;;

  let%expect_test _ =
    parse_test {| let a : int = 2 ++ 2 ** 2 |};
    [%expect {| let (a : int) = (2 ++ (2 ** 2)) |}]
  ;;

  let%expect_test _ =
    parse_test {| let a = (2 ++ 2) ** 2 |};
    [%expect {| let a = ((2 ++ 2) ** 2) |}]
  ;;

  let%expect_test _ =
    parse_test {| let a : int = let f x = x in 13 |};
    [%expect {| 
      let (a : int) = let f = (fun x -> x)
      in 13
      |}]
  ;;

  let%expect_test _ =
    parse_test {| let ( += ) = x -$ 1 |};
    [%expect {| let ( += ) = (x -$ 1) |}]
  ;;

  let%expect_test _ =
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

  let%expect_test _ =
    parse_test {| let a = (+) x y |};
    [%expect {| let a = (x + y) |}]
  ;;

  let%expect_test _ =
    parse_test {| let ( += ) a b = (~-) 1 |};
    [%expect {| let ( += ) = (fun a -> (fun b -> ( ~- ) 1)) |}]
  ;;

  let%expect_test _ =
    parse_test {| let f (lst: (int -> int)) = g |};
    [%expect {| let f = (fun (lst : int -> int) -> g) |}]
  ;;

  let%expect_test _ =
    parse_test {| let a = let f x = x + 1 in let b = 5 in f (b + 1) |};
    [%expect
      {| 
      let a = let f = (fun x -> (x + 1))
      in let b = 5
      in f (b + 1)
    |}]
  ;;

  let%expect_test _ =
    parse_test "let a =([3;3], [4;4]) :: [([5;5],[6;6]);([7;7],[8;8])]";
    [%expect
      {| let a = ((3 :: 3 :: []), (4 :: 4 :: [])) :: ((5 :: 5 :: []), (6 :: 6 :: [])) :: ((7 :: 7 :: []), (8 :: 8 :: [])) :: [] |}]
  ;;

  let%expect_test _ =
    parse_test
      {|
    let a = 
      match l with 
      | (hd, tl) -> hd 
      | _ -> 0
    |};
    [%expect {|
      let a = match l with
      | (hd, tl) -> hd
      | _ -> 0|}]
  ;;

  let%expect_test _ =
    parse_test {| let f (lst: ('b list)) = lst |};
    [%expect {| let f = (fun (lst : 'b list) -> lst) |}]
  ;;

  let%expect_test _ =
    parse_test "let a = - 3";
    [%expect {| let a = - 3 |}]
  ;;

  let%expect_test _ =
    parse_test "let a = 3 -> 3";
    [%expect {| Syntax error |}]
  ;;

  let%expect_test _ =
    parse_test "let a = f a + f b";
    [%expect {| let a = (f a + f b) |}]
  ;;

  let%expect_test _ =
    parse_test "let a = - f 3 - f 4";
    [%expect {| let a = (- f 3 - f 4) |}]
  ;;

  let%expect_test _ =
    parse_test "let f (hd::tl) = 3";
    [%expect {| let f = (fun hd :: tl -> 3) |}]
  ;;

  let%expect_test _ =
    parse_test "let f (x,y) = x + y";
    [%expect {| let f = (fun (x, y) -> (x + y)) |}]
  ;;

  let%expect_test _ =
    parse_test "let f ((x: 'a),y) =  x * y";
    [%expect {| let f = (fun ((x : 'a), y) -> (x * y)) |}]
  ;;

  let%expect_test _ =
    parse_test "let f ((g : (int -> int)),y) = g y + g (y + 1)";
    [%expect {| let f = (fun ((g : int -> int), y) -> (g y + g (y + 1))) |}]
  ;;

  let%expect_test _ =
    parse_test "let f (x :: (x, y)) = 3";
    [%expect {| let f = (fun x :: (x, y) -> 3) |}]
  ;;

  let%expect_test _ =
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
      in f 10 |}]
  ;;

  let%expect_test _ =
    parse_test "let f (x, (y :: ys)) = 3";
    [%expect {| let f = (fun (x, (y :: ys)) -> 3) |}]
  ;;

  let%expect_test _ =
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

  let%expect_test _ =
    parse_test "let a = let f x = x in f (f (f (5 + f 5+5+5+f 5) + 5) + 5) + 5";
    [%expect
      {|
      let a = let f = (fun x -> x)
      in (f (f (f ((((5 + f 5) + 5) + 5) + f 5) + 5) + 5) + 5) |}]
  ;;
end

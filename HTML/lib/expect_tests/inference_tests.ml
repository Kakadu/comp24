(** Copyright 2024-2025, David Akhmedov, Danil Parfyonov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module InferenceTests = struct
  open TypeInference

  let infer_test s =
    match Parser.parse_program s with
    | Ok actual ->
      let env = Inferencer.run_inference actual in
      Inferencer.print_env env
    | Error err -> Format.printf "%s\n" err
  ;;

  let%expect_test "sanity check" =
    infer_test {| let a = 3 |};
    [%expect {| val a : int |}]
  ;;

  let%expect_test "Base minus" =
    infer_test {| 
      let ( - ) a = a = 100;;
      let a = (+) (-7) 5;;
    |};
    [%expect {| 
    val - : int -> bool
    val a : int 
    |}]
  ;;

  let%expect_test "Use redefined minus" =
    infer_test {| 
      let ( - ) a = a = 100
      let a = (+) ((-) 7) 5;;
    |};
    [%expect
      {|  Typecheck error: This expression has type bool but an expression was expected of type int |}]
  ;;

  let%expect_test "wrong annotated type" =
    infer_test {| let a: bool = 3 |};
    [%expect
      {| Typecheck error: This expression has type int but an expression was expected of type bool |}]
  ;;

  let%expect_test "wrong annotated type" =
    infer_test {| let a (x: int): bool = 3 |};
    [%expect
      {| Typecheck error: This expression has type int but an expression was expected of type bool |}]
  ;;

  let%expect_test "operator definiton" =
    infer_test {| 
  let ( -$ ) a b = ( / ) a b|};
    [%expect {| 
    val -$ : int -> int -> int
    |}]
  ;;

  let%expect_test "operator application" =
    infer_test {| let a = ( + ) 5|};
    [%expect {| val a : int -> int |}]
  ;;

  let%expect_test "operator application and redefinition" =
    infer_test {| 
  let a = ( + ) 5;; 
  let ( + ) = (fun x y -> x || y);;|};
    [%expect {|
    val + : bool -> bool -> bool
    val a : int -> int |}]
  ;;

  let%expect_test "wrong pattern" =
    infer_test {|let f (x :: (x, y)) = 3|};
    [%expect
      {|
    Typecheck error: This expression has type 'a list but an expression was expected of type 'b * 'c |}]
  ;;

  let%expect_test "polymorphic type" =
    infer_test {|let f (a: 'a) = a |};
    [%expect {| val f : 'a -> 'a |}]
  ;;

  let%expect_test "polymorphic annotated arguments' type" =
    infer_test
      {|let f (g: ('a -> 'b)) (x: 'b) = 100
  let res = f (fun x -> true) false;;|};
    [%expect {|
      val f : ('a -> 'b) -> 'b -> int
      val res : int |}]
  ;;

  let%expect_test "polymorphic annotated types unification error" =
    infer_test {|let f (g: ('a -> 'b)) (x: 'b) = 100
    let res = f (fun x -> true) 5;;|};
    [%expect
      {| Typecheck error: This expression has type int but an expression was expected of type bool |}]
  ;;

  let%expect_test "convoluted example" =
    infer_test {|let f (g: ('a -> 'a)) (h: ('c -> 'b)) (x: 'b) = g h x;;|};
    [%expect {| val f : (('a -> 'a) -> 'a -> 'a) -> ('a -> 'a) -> 'a -> 'a |}]
  ;;

  let%expect_test "polymorphic annotated arguments' type" =
    infer_test {|let f (h: ('c -> 'b)) (x: 'b) = h x;;|};
    [%expect {| val f : ('a -> 'a) -> 'a -> 'a |}]
  ;;

  let%expect_test "match pattern" =
    infer_test
      {| let rec map f lst = 
      match lst with 
        | [] -> []
        | (x::xs) -> f x :: map f xs |};
    [%expect {| val map : ('a -> 'b) -> 'a list -> 'b list |}]
  ;;

  let%expect_test "list of tuple of two int lists" =
    infer_test {| let a =([3;3], [4;4]) :: [([5;5],[6;6]);([7;7],[8;8])] |};
    [%expect {| val a : (int list * int list) list |}]
  ;;

  let%expect_test "multiple let separated by ;;, pattern matching" =
    infer_test
      {| let f x = x;; let a = match [3;4;5] with | (hd::tl) -> (2, f true) | hd -> (f 2, false) |};
    [%expect {|
    val a : int * bool
    val f : 'a -> 'a
|}]
  ;;

  let%expect_test "odd even mutual recursion" =
    infer_test
      {|let rec even x =
        if x = 0 then true
        else odd (x - 1)
      and odd x =
        if x = 0 then false
        else even (x - 1)
;;|};
    [%expect {|
      val even : int -> bool
      val odd : int -> bool |}]
  ;;

  let%expect_test "mutual recursion variable definiton" =
    infer_test {|let rec x = [1, 2, 3]
    and y = [x]|};
    [%expect
      {|
      val x : (int * int * int) list
      val y : (int * int * int) list list |}]
  ;;

  let%expect_test "let ... and ... variable definition error" =
    infer_test {|let x = [1, 2, 3]
    and y = [x]|};
    [%expect {|
    Typecheck error: Unbound value x |}]
  ;;

  let%expect_test "let ... and ... variable definition correct" =
    infer_test {|let f (a: int): int = a
and g (b: bool): bool = b;;|};
    [%expect {|
  val f : int -> int
  val g : bool -> bool |}]
  ;;

  let%expect_test "mutual recursive functions polymorphic" =
    infer_test {|let rec id = fun x -> x and dup x y = (id x, id y);;|};
    [%expect
      {| 
            val dup : 'a -> 'a -> 'a * 'a
            val id : 'a -> 'a
            |}]
  ;;

  let%expect_test "mutual recursive functions wrong type" =
    infer_test {|let rec f x = x 
and g () = f 2 
and h () = f true;;|};
    [%expect
      {|
Typecheck error: This expression has type bool but an expression was expected of type int |}]
  ;;

  let%expect_test "mutual recursive functions" =
    infer_test {|let rec f x = g x 1 
                and g x y = x + y;;|};
    [%expect
      {| 
          val f : int -> int
          val g : int -> int -> int
          |}]
  ;;

  let%expect_test "factorial" =
    infer_test
      {|let rec factorial = fun (n: int) -> 
  if n = 0 then 1 
  else n * factorial (n - 1);;|};
    [%expect {| 
val factorial : int -> int
|}]
  ;;

  let%expect_test "mutual recursive functions" =
    infer_test
      {|let rec f1 x = f2 (x - 1)

and f2 x = f3 (x - 1)

and f3 x = f1 (x - 1)

let result = f1 5;;|};
    [%expect
      {| 
val f1 : int -> 'a
val f2 : int -> 'a
val f3 : int -> 'a
val result : 'a
|}]
  ;;

  let%expect_test "mutual recursive operators" =
    infer_test {|let rec ( += ) x = x -$ 1 
and (-$) = fun x y -> ( + ) x y;;|};
    [%expect {| 
val += : int -> int
val -$ : int -> int -> int
|}]
  ;;

  let%expect_test "polymorphic arguments' type" =
    infer_test
      {|let final_value (x: 'a) (y: 'b) =
  let double_x: int = x * 2 in
  let incremented_y: int = y + 1 in
  let result: int = double_x + incremented_y in
  result * result
;;|};
    [%expect {| 
val final_value : int -> int -> int
|}]
  ;;

  let%expect_test "nested let" =
    infer_test {|let a = let f x = x + 1 in let b = 5 in f (b + 1);;|};
    [%expect {| 
val a : int
|}]
  ;;

  let%expect_test "occurs check" =
    infer_test {|let f x = x x;;|};
    [%expect {| 
Typecheck error: Occurs check failed
|}]
  ;;

  let%expect_test "y combinator lambda" =
    infer_test {|let a = fun f -> (fun x -> f (x x)) (fun x -> f (x x));;|};
    [%expect {| 
Typecheck error: Occurs check failed
|}]
  ;;

  let%expect_test "equals boolean operator" =
    infer_test {|let f x y = (=) x y;;|};
    [%expect {| 
val f : 'a -> 'a -> bool
|}]
  ;;

  let%expect_test "or boolean operator" =
    infer_test {|let f x y = (x > false) || (y > 2);;|};
    [%expect {| 
val f : bool -> int -> bool
|}]
  ;;

  let%expect_test "unary and binary operator" =
    infer_test "let a = +4 + (-3)";
    [%expect {| val a : int |}]
  ;;

  let%expect_test "lhs let ... and ... recursion error" =
    infer_test "let rec (x, y, z) = (1, 2, 3) and (g, h) = (5, 6)";
    [%expect
      {| Typecheck error: Only variables are allowed as left-hand side of `let rec' |}]
  ;;

  let%expect_test "let ... and ..." =
    infer_test "let (x, y, z) = (1, 2, 3) and (g, h) = (5, 6)";
    [%expect
      {|
      val g : int
      val h : int
      val x : int
      val y : int
      val z : int |}]
  ;;

  let%expect_test "annotated expression" =
    infer_test "let x = ((fun y -> y + 1): (int -> int))";
    [%expect {| val x : int -> int |}]
  ;;

  let%expect_test "005fix" =
    infer_test "let rec fix = (fun f -> (fun x -> ((f (fix f)) x)));;";
    [%expect {| val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b |}]
  ;;

  let%expect_test "lhs recursion error" =
    infer_test "let rec (x, y, z) = (1, 2, 3)";
    [%expect
      {| Typecheck error: Only variables are allowed as left-hand side of `let rec' |}]
  ;;

  let%expect_test "wrong annotated type 005fix" =
    infer_test "let rec fix: (int -> int) = (fun f -> (fun x -> ((f (fix f)) x)));;";
    [%expect
      {| Typecheck error: This expression has type int -> 'e but an expression was expected of type int |}]
  ;;

  let%expect_test "recursive unary operator" =
    infer_test "let rec ( +& ) x  = 1 :: ((+&)x);;";
    [%expect {| val +& : 'a -> int list |}]
  ;;

  let%expect_test "pattern" =
    infer_test {|let dam (_, (x::xs)) = x|};
    [%expect {| val dam : 'a * 'b list -> 'b |}]
  ;;
end

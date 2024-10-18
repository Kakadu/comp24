(** Copyright 2023-2024, Nikita Lukonenko and Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Shaitanml_lib
open Interpreter

let%expect_test _ =
  test_interpret
    {|
      let rec fac n = if n < 1 then 1 else n * fac (n - 1);;
      let x = fac 5;;
    |};
  [%expect {|
    val fac : int -> int = <fun>
    val x : int = 120 |}]
;;

let%expect_test _ =
  test_interpret
    {|
    let rec fix f x = f (fix f) x;;
    let fac fac_ n = if n < 1 then 1 else n * fac_ (n - 1);;
    let f = fix fac 5;;
    |};
  [%expect
    {|
    val f : int = 120
    val fac : (int -> int) -> int -> int = <fun>
    val fix : (('2 -> '3) -> '2 -> '3) -> '2 -> '3 = <fun> |}]
;;

let%expect_test _ =
  test_interpret
    {|
    let x =
      let y =
        let z =
          let w = 1
          in w
        in z
      in y
    |};
  [%expect {|
    val x : int = 1 |}]
;;

let%expect_test _ =
  test_interpret
    {|
    let a =
      let b =
        let rec f = (let x = 3 in x) + 1
        in f
      in ();;
    let s = "string";;
    |};
  [%expect {|
    val a : unit = ()
    val s : string = string |}]
;;

let%expect_test _ =
  test_interpret
    {|
    let x =
      let rec fac n = if n = 1 then 1 else n * fac (n - 1) in
      fac 5
    ;;
    |};
  [%expect {|
    val x : int = 120 |}]
;;

let%expect_test _ =
  test_interpret
    {|
    let rev l =
      let rec helper acc xs =
        match xs with
        | [] -> acc
        | h :: tl -> helper (h :: acc) tl
      in
      helper [] l
    ;;

    let reversed = rev [1;2;3;4;5];;
    |};
  [%expect
    {|
    val rev : '12 list -> '12 list = <fun>
    val reversed : int list = [5; 4; 3; 2; 1] |}]
;;

let%expect_test _ =
  test_interpret
    {|
    let f a b c d e = a b c d e;;
    let id x = x;;

    let cmp = f id id id id (fun x -> x + 1);;
    |};
  [%expect
    {|
    val cmp : int -> int = <fun>
    val f : ('1 -> '2 -> '3 -> '4 -> '5) -> '1 -> '2 -> '3 -> '4 -> '5 = <fun>
    val id : '9 -> '9 = <fun> |}]
;;

let%expect_test _ =
  test_interpret
    {|
  let arith x y = (x * y, x / y, x + y, x - y);;
  let prod x y =
    let fst (a, _, _, _) = a in
    fst (arith x y)
  ;;
  let p = prod 3 0;;
    |};
  [%expect {|
    Interpreter error: Division by zero |}]
;;

let%expect_test _ =
  test_interpret
    {|
    let f l = match l with
      | x :: y :: tl -> x + y
      | x :: y :: z :: tl -> x + y + z
    ;;
    let x = f [1];;
    |};
  [%expect {|
    Interpreter error: Pattern-matching failed |}]
;;

let%expect_test _ =
  test_interpret {|
    let (x, y, z) = (1, 2, 3, 4);;
    |};
  [%expect {|
    Infer error: Not implemented |}]
;;

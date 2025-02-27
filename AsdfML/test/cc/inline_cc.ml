(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Lib
open Test.Utils

let test_cc code =
  let open Format in
  closure_conversion code (printf "%a" Sast.pp_program)
;;

let%expect_test _ =
  test_cc "let test = fun x -> x";
  [%expect {| let test = (fun x -> x) |}]
;;

let%expect_test _ =
  test_cc
    {|
    let test1 = fun x -> fun y -> y x
    let test2 = fun x y -> y x
    let test3 = fun x -> (fun x y -> y x) x
    |};
  [%expect
    {|
    let test1 = (fun x y -> (y x))
    let test2 = (fun x_0 y_0 -> (y_0 x_0))
    let test3 = (fun x_2 y_1 -> (y_1 x_2))
    |}]
;;

let%expect_test _ =
  test_cc
    {|
      let mk_add_1 = fun a -> fun b -> fun c -> a + b + c
      let mk_add_2 = fun a b c -> a + b + c
   |};
  [%expect
    {|
    let mk_add_1 = (fun a b c -> (( + ) (( + ) a b) c))
    let mk_add_2 = (fun a_0 b_0 c_0 -> (( + ) (( + ) a_0 b_0) c_0))
    |}]
;;

let%expect_test _ =
  test_cc
    {|
      let fact n =
        let rec helper n cont =
          if n <= 1 then 
            cont 1
          else 
            helper (n - 1) (fun res -> cont (n * res)) 
        in 
        helper n (fun x -> x)
    |};
  [%expect
    {|
    let fact = (fun n ->
      let rec helper = (fun n_0 cont ->
        if (( <= ) n_0 1)
        then (cont 1)
        else (helper (( - ) n_0 1) ((fun cont n_0 res ->
          (cont (( * ) n_0 res))) cont n_0))) in
      (helper n (fun x -> x)))
    |}]
;;

let%expect_test _ =
  test_cc
    {|
    let gen seed1 seed2 = 
      let gen n = n * seed2 + seed1 * 42 in
      [gen 1; gen 2; gen 3]
    |};
  [%expect
    {|
    let gen = (fun seed1 seed2 ->
      let gen_0 = (fun seed1 seed2 n -> (( + ) (( * ) n seed2) (( * ) seed1 42))) in
      [(gen_0 seed1 seed2 1); (gen_0 seed1 seed2 2); (gen_0 seed1 seed2 3)])
    |}]
;;

let%expect_test _ =
  test_cc
    {|
    let main x = 
      let const f = fun s -> f in
      let rev_const f s = const s in
      rev_const (fun _ -> x)
    |};
  [%expect
    {|
    let main = (fun x ->
      let const = (fun f s -> f) in
      let rev_const = (fun const f_0 s_0 -> (const s_0)) in
      (rev_const const ((fun x _ -> x) x)))
    |}]
;;

let%expect_test _ =
  test_cc
    {|
    let add_cps x y = fun k -> k (x + y)
    let square_cps x = fun k -> k (x * x)
    let pythagoras_cps x y = fun k ->
      square_cps x (fun x_squared ->
        square_cps y (fun y_squared ->
          add_cps x_squared y_squared k))
    |};
  [%expect
    {|
    let add_cps = (fun x y k -> (k (( + ) x y)))
    let square_cps = (fun x_0 k_0 -> (k_0 (( * ) x_0 x_0)))
    let pythagoras_cps = (fun x_1 y_0 k_1 ->
      (square_cps x_1 ((fun k_1 y_0 x_squared ->
      (square_cps y_0 ((fun k_1 x_squared y_squared ->
      (add_cps x_squared y_squared k_1)) k_1 x_squared))) k_1 y_0)))
    |}]
;;

let%expect_test _ =
  test_cc
    {|
    let shitty_mul x =
      let rec helper acc cnt = 
        if cnt = 0 then acc
        else helper (acc+x) (cnt-1)
      in helper 0;;
    |};
  [%expect
    {|
    let shitty_mul = (fun x ->
      let rec helper = (fun x acc cnt ->
        if (( = ) cnt 0)
        then acc
        else (helper x (( + ) acc x) (( - ) cnt 1))) in
      (helper x 0))
    |}]
;;

let%expect_test _ =
  test_cc
    {|
    let add x y = x + y
    let add1 = let one = 1 in (fun x -> one + x)
    |};
  [%expect
    {|
    let add = (fun x y -> (( + ) x y))
    let add1 = let one = 1 in
      ((fun one x_0 -> (( + ) one x_0)) one)
    |}]
;;

(* let%expect_test _ =
  test_cc
    {|
    let shitty_mul x =
      let rec helper =
        let zero = 0 in
        fun acc cnt -> if cnt = zero then acc else helper (acc + x) (cnt - 1)
      in
      helper 0
  |};
  [%expect {| |}]
;; *)

let%expect_test _ =
  test_cc
    {|
    let a c d =
      let m = c + d in
      let k l = l + m in
      k (5 + m)
    |};
  [%expect
    {|
    let a = (fun c d ->
      let m = (( + ) c d) in
      let k = (fun m l -> (( + ) l m)) in
      (k m (( + ) 5 m)))
    |}]
;;

let%expect_test _ =
  test_cc
    {|
    let test x = 
      let one = 
        let z=0 in 
        (fun x -> 1) 
      in
      let two = 2 in
      let add_three x = x + two + (one ())
      in add_three x
    |};
  [%expect
    {|
    let test = (fun x ->
      let one = let z = 0 in
        (fun x_0 -> 1) in
      let two = 2 in
      let add_three = (fun one two x_1 -> (( + ) (( + ) x_1 two) (one ()))) in
      (add_three one two x))
    |}]
;;

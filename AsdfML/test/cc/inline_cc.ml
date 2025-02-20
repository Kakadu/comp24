(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Lib

let test_cc code =
  let open Format in
  match Parser.parse_program code with
  | Error e -> print_endline e
  | Ok ast ->
    (match Inferencer.inference_program ast with
     | Error e -> printf "%a" Pp_typing.pp_error e
     | Ok ast ->
       let ast =
         ast
         |> Tast.strip_types_program
         |> Remove_patterns.remove_patterns
         |> Remove_match.remove_match
         |> Closure_conversion.closure_conversion
       in
       printf "%a" Sast.pp_program ast)
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
    let test2 = (fun x y -> (y x))
    let test3 = (fun x y -> (y x))
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
    let mk_add_2 = (fun a b c -> (( + ) (( + ) a b) c))
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
      let rec helper = (fun n cont ->
        if (( <= ) n 1)
        then (cont 1)
        else (helper (( - ) n 1) ((fun cont n res ->
          (cont (( * ) n res))) cont n))) in
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
      let gen = (fun seed1 seed2 n -> (( + ) (( * ) n seed2) (( * ) seed1 42))) in
      [(gen seed1 seed2 1); (gen seed1 seed2 2); (gen seed1 seed2 3)])
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
      let rev_const = (fun const f s -> (const s)) in
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
    let square_cps = (fun x k -> (k (( * ) x x)))
    let pythagoras_cps = (fun x y k -> (square_cps x ((fun k y x_squared ->
      (square_cps y ((fun k x_squared y_squared ->
      (add_cps x_squared y_squared k)) k x_squared))) k y)))
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
      ((fun one x -> (( + ) one x)) one)
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
        (fun x -> 1) in
      let two = 2 in
      let add_three = (fun one two x -> (( + ) (( + ) x two) (one ()))) in
      (add_three one two x))
    |}]
;;

(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Lib
open Test.Utils

let test code =
  let open Format in
  remove_patterns code (printf "\n%a" Pp_ast.pp_program )
;;

let%expect_test _ =
  test {|
    let const _ = 42 
  |};
  [%expect {| let const = (fun _ -> 42) |}]
;;

let%expect_test _ =
  test {|
    let tuple_sum (a, b) = a + b
  |};
  [%expect
    {|
    let tuple_sum = (fun arg_0 -> match arg_0 with
       | (a, b) -> (( + ) a b))
    |}]
;;

let%expect_test _ =
  test {|
    let tuple_sum (a, (b, c)) = a + b + c
  |};
  [%expect
    {|
    let tuple_sum = (fun arg_0 ->
       match arg_0 with
       | (a, (b, c)) -> (( + ) (( + ) a b) c))
    |}]
;;

let%expect_test _ =
  test {|
    let f (a, (b, c)) = a + b + c
  |};
  [%expect
    {|
    let f = (fun arg_0 ->
       match arg_0 with
       | (a, (b, c)) -> (( + ) (( + ) a b) c))
    |}]
;;

let%expect_test _ =
  test {|
    let f [[a; b]; [c; d]] = a + b + c + d
  |};
  [%expect
    {|
    let f = (fun arg_0 ->
       match arg_0 with
       | [[a; b]; [c; d]] -> (( + ) (( + ) (( + ) a b) c) d))
    |}]
;;

let%expect_test _ =
  test
    {|
    let _ = 
      let tuple_sum = fun (a, b) -> a + b in 
      tuple_sum (1, 2)
  |};
  [%expect
    {|
    let _ =
       let tuple_sum = (fun arg_0 -> match arg_0 with
         | (a, b) -> (( + ) a b)) in
       (tuple_sum (1, 2))
    |}]
;;

let%expect_test _ =
  test {|
    let list_sum [a;b] = a + b 
  |};
  [%expect
    {|
    let list_sum = (fun arg_0 -> match arg_0 with
       | [a; b] -> (( + ) a b))
    |}]
;;

let%expect_test _ =
  test {|
    let _ = 
      let (x, y) = (1, 2) in 
      x + y
  |};
  [%expect {|
    let _ = match (1, 2) with
       | (x, y) -> (( + ) x y)
    |}]
;;

let%expect_test _ =
  test {|
    let _ = 
      let v = (1, 2) in 
      let (x, y) = v in
      x + y
  |};
  [%expect
    {|
    let _ = let v = (1, 2) in
       match v with
       | (x, y) -> (( + ) x y)
    |}]
;;

let%expect_test _ =
  test {|
    let _ = 
      let [x; y] = [1; 2] in
      x + y
  |};
  [%expect {|
    let _ = match [1; 2] with
       | [x; y] -> (( + ) x y)
    |}]
;;

let%expect_test _ =
  test
    {|
    let fst vec = 
      let helper = (fun (x, y, z) -> x) in 
      helper vec
  |};
  [%expect
    {|
    let fst = (fun vec ->
       let helper = (fun arg_0 -> match arg_0 with
         | (x, y, z) -> x) in
       (helper vec))
    |}]
;;

let%expect_test _ =
  test {|
    let (a, b, c) = (1, 2, 3)
  |};
  [%expect
    {|
    let __temp_match_0 = let __temp_match_0 = (1, 2, 3) in
       match __temp_match_0 with
       | (a, b, c) -> __temp_match_0
    |}]
;;

let%expect_test _ =
  test {|
    let [a; b; c] = [1; 2; 3; 4]
  |};
  [%expect
    {|
    let __temp_match_0 = let __temp_match_0 = [1; 2; 3; 4] in
       match __temp_match_0 with
       | [a; b; c] -> __temp_match_0
    |}]
;;

let%expect_test _ =
  test {|
    let a::b = [1; 2; 3; 4]
  |};
  [%expect
    {|
    let __temp_match_0 = let __temp_match_0 = [1; 2; 3; 4] in
       match __temp_match_0 with
       | a :: b -> __temp_match_0
    |}]
;;

let%expect_test _ =
  test
    {|
    let cross (x1, y1, z1) (x2, y2, z2) =
      let x = (y1 * z2) - (z1 * y2) in
      let y = (z1 * x2) - (x1 * z2) in
      let z = (x1 * y2) - (y1 * x2) in
      (x, y, z)
  |};
  [%expect
    {|
    let cross = (fun arg_0 arg_1 ->
       match arg_1 with
       | (x2, y2, z2) -> (
         match arg_0 with
         | (x1, y1, z1) -> (let x = (( - ) (( * ) y1 z2) (( * ) z1 y2)) in
           let y = (( - ) (( * ) z1 x2) (( * ) x1 z2)) in
           let z = (( - ) (( * ) x1 y2) (( * ) y1 x2)) in
           (x, y, z))))
    |}]
;;

let%expect_test _ =
  test {|
    let (a, (b, c), [d;e], f::g) = (1, (2, 3), [4; 5], [6; 7]) 
  |};
  [%expect
    {|
    let __temp_match_0 = let __temp_match_0 = (1, (2, 3), [4; 5], [6; 7]) in
       match __temp_match_0 with
       | (a, (b, c), [d; e], f :: g) -> __temp_match_0
    |}]
;;

let%expect_test _ =
  test {|
    let test arg_0 (a, b) arg_1 = a + b
  |};
  [%expect
    {|
    let test = (fun arg_0 arg_2 arg_1 ->
       match arg_2 with
       | (a, b) -> (( + ) a b))
    |}]
;;

let%expect_test _ =
  test {|
    let __temp_match_0 = 42
    let x = __temp_match_0
    let (a, b, c) = (1, 2, 3)
    let y = __temp_match_0
  |};
  [%expect
    {|
    let __var__temp_match_0 = 42
    let x = __var__temp_match_0
    let __temp_match_0 = let __temp_match_0 = (1, 2, 3) in
      match __temp_match_0 with
      | (a, b, c) -> __temp_match_0
    let y = __var__temp_match_0
    |}]
;;

let%expect_test _ =
  test
    {|
    let pow x n =
      let rec helper acc n =
        match n with
        | 0 -> acc
        | n -> helper (acc * x) (n - 1)
      in
      helper 1 n
  |};
  [%expect {|
    let pow = (fun x n ->
       let rec helper = (fun acc n_0 -> match n_0 with
         | 0 -> acc
         | n_1 -> (helper (( * ) acc x) (( - ) n_1 1))) in
       (helper 1 n))
    |}]
;;

(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Lib
open Base
open Format
open Test.Utils

let test code = anf code (printf "%a" Anf_ast.pp_program)

let%expect_test _ =
  test {|
    let a = 42
  |};
  [%expect {| 
    let a = 42
  |}]
;;

let%expect_test _ =
  test {|
    let a = 1 + 2 - 42
  |};
  [%expect {|
    let a = let anf1 = ( + ) 1 2 in
      ( - ) anf1 42
    |}]
;;

let%expect_test _ =
  test {|
    let a = 
      let one = 1 in 
      let two = 2 in
      one + two
  |};
  [%expect {|
    let a = let one = 1 in
      let two = 2 in
      ( + ) one two
    |}]
;;

let%expect_test _ =
  test
    {|
    let rec fact = fun x -> if x < 2 then 1 else x * fact (x - 1)
    let main = print_int (fact 5)
  |};
  [%expect
    {|
    let fact x =
      let anf1 = ( < ) x 2 in
      if anf1
      then 1
      else let anf4 = ( - ) x 1 in
        let anf3 = fact anf4 in
        ( * ) x anf3
    let main = let anf6 = fact 5 in
      print_int anf6
    |}]
;;

let%expect_test _ =
  test
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
    let ll_2 cont n_0 res = let anf1 = ( * ) n_0 res in
      cont anf1
    let ll_helper_1 n_0 cont =
      let anf3 = ( <= ) n_0 1 in
      if anf3
      then cont 1
      else
        let anf6 = ( - ) n_0 1 in
        let anf7 = ll_2 cont n_0 in
        ll_helper_1 anf6 anf7
    let ll_3 x = x
    let fact n = ll_helper_1 n ll_3
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
    
    let main = 
      let a = (1, 2, 3) in
      let b = (4, 5, 6) in
      let c = cross a b in
      print_tuple c
    |};
  [%expect
    {|
    let cross arg_0 arg_1 =
      let x2 = ml_get_tuple_field arg_1 0 in
      let y2 = ml_get_tuple_field arg_1 1 in
      let z2 = ml_get_tuple_field arg_1 2 in
      let x1 = ml_get_tuple_field arg_0 0 in
      let y1 = ml_get_tuple_field arg_0 1 in
      let z1 = ml_get_tuple_field arg_0 2 in
      let anf4 = ( * ) y1 z2 in
      let anf5 = ( * ) z1 y2 in
      let x = ( - ) anf4 anf5 in
      let anf2 = ( * ) z1 x2 in
      let anf3 = ( * ) x1 z2 in
      let y = ( - ) anf2 anf3 in
      let anf0 = ( * ) x1 y2 in
      let anf1 = ( * ) y1 x2 in
      let z = ( - ) anf0 anf1 in
      (x, y, z)
    let main =
      let a = (1, 2, 3) in
      let b = (4, 5, 6) in
      let c = cross a b in
      print_tuple c
    |}]
;;

let%expect_test _ =
  test
    {|
    let rec map f list = match list with
      | hd :: tl -> (f hd) :: (map f tl) 
      | [] -> []

    let rec map_ f list = match list with
      | hd :: tl -> f hd :: map f tl
      | _ -> []
  |};
  [%expect
    {|
    let map f list =
      let anf8 = ml_list_is_empty list in
      let anf1 = not anf8 in
      if anf1
      then
        let hd = ml_list_hd list in
        let tl = ml_list_tl list in
        let anf3 = f hd in
        let anf4 = map f tl in
        ( :: ) anf3 anf4
      else let anf6 = ml_list_is_empty list in
        if anf6
        then []
        else panic ()
    let map_ f_0 list_0 =
      let anf14 = ml_list_is_empty list_0 in
      let anf10 = not anf14 in
      if anf10
      then
        let hd_0 = ml_list_hd list_0 in
        let tl_0 = ml_list_tl list_0 in
        let anf12 = f_0 hd_0 in
        let anf13 = map f_0 tl_0 in
        ( :: ) anf12 anf13
      else []
    |}]
;;

let%expect_test _ =
  test
    {| 
  let fib n =
    let rec helper acc n =
      match (n, acc) with
      | (0, x :: _) -> x
      | (_, x :: y :: _) -> helper ((x + y) :: acc) (n - 1)
      | _ -> -1
    in
    helper [ 1; 1 ] (n - 2)
  |};
  [%expect
    {|
    let ll_helper_1 acc n_0 =
      let anf23 = ml_get_tuple_field (n_0, acc) 0 in
      let anf19 = ( = ) anf23 0 in
      let anf22 = ml_get_tuple_field (n_0, acc) 1 in
      let anf21 = ml_list_is_empty anf22 in
      let anf20 = not anf21 in
      let anf1 = ( && ) anf19 anf20 in
      if anf1
      then
        let __tuple_1 = (n_0, acc) in
        let anf2 = ml_get_tuple_field __tuple_1 1 in
        ml_list_hd anf2
      else
        let anf18 = ml_get_tuple_field (n_0, acc) 1 in
        let anf17 = ml_list_is_empty anf18 in
        let anf12 = not anf17 in
        let anf16 = ml_get_tuple_field (n_0, acc) 1 in
        let anf15 = ml_list_tl anf16 in
        let anf14 = ml_list_is_empty anf15 in
        let anf13 = not anf14 in
        let anf4 = ( && ) anf12 anf13 in
        if anf4
        then
          let __tuple_0 = (n_0, acc) in
          let anf11 = ml_get_tuple_field __tuple_0 1 in
          let x_0 = ml_list_hd anf11 in
          let anf10 = ml_get_tuple_field __tuple_0 1 in
          let anf9 = ml_list_tl anf10 in
          let y = ml_list_hd anf9 in
          let anf8 = ( + ) x_0 y in
          let anf6 = ( :: ) anf8 acc in
          let anf7 = ( - ) n_0 1 in
          ll_helper_1 anf6 anf7
        else -1
    let fib n = let anf25 = ( - ) n 2 in
      ll_helper_1 [1; 1] anf25
    |}]
;;

let%expect_test _ =
  test
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
    let add_cps x y k = let anf1 = ( + ) x y in
      k anf1
    let square_cps x_0 k_0 = let anf3 = ( * ) x_0 x_0 in
      k_0 anf3
    let ll_4 k_1 x_squared y_squared = add_cps x_squared y_squared k_1
    let ll_3 k_1 y_0 x_squared =
      let anf6 = ll_4 k_1 x_squared in
      square_cps y_0 anf6
    let pythagoras_cps x_1 y_0 k_1 =
      let anf8 = ll_3 k_1 y_0 in
      square_cps x_1 anf8
    |}]
;;

let%expect_test _ =
  test {|
    let f a0 a1 a2 a3 a4 a5 = a0 + a1 + a2 + a3 + a4 + a5
    |};
  [%expect
    {|
    let f a0 a1 a2 a3 a4 a5 =
      let anf4 = ( + ) a0 a1 in
      let anf3 = ( + ) anf4 a2 in
      let anf2 = ( + ) anf3 a3 in
      let anf1 = ( + ) anf2 a4 in
      ( + ) anf1 a5
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
  [%expect
    {|
    let ll_helper_1 x acc n_0 =
      let anf1 = ( = ) n_0 0 in
      if anf1
      then acc
      else
        let anf3 = ( * ) acc x in
        let anf4 = ( - ) n_0 1 in
        ll_helper_1 x anf3 anf4
    let pow x n = ll_helper_1 x 1 n
    |}]
;;

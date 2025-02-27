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
    let a = let a1 = ( + ) 1 2 in
      ( - ) a1 42
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
    let a = let a0 = 1 in
      let a1 = 2 in
      ( + ) a0 a1
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
      let a1 = ( < ) x 2 in
      if a1
      then 1
      else let a4 = ( - ) x 1 in
        let a3 = fact a4 in
        ( * ) x a3
    let main = let a6 = fact 5 in
      print_int a6
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
    let __ll_2 cont n_0 res = let a1 = ( * ) n_0 res in
      cont a1
    let __ll_helper_1 n_0 cont =
      let a3 = ( <= ) n_0 1 in
      if a3
      then cont 1
      else let a6 = ( - ) n_0 1 in
        let a7 = __ll_2 cont n_0 in
        __ll_helper_1 a6 a7
    let __ll_3 x = x
    let fact n = __ll_helper_1 n __ll_3
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
      let a2 = `get_tuple_field arg_1 0 in
      let a4 = `get_tuple_field arg_1 1 in
      let a6 = `get_tuple_field arg_1 2 in
      let a9 = `get_tuple_field arg_0 0 in
      let a11 = `get_tuple_field arg_0 1 in
      let a13 = `get_tuple_field arg_0 2 in
      let a24 = ( * ) a11 a6 in
      let a25 = ( * ) a13 a4 in
      let a15 = ( - ) a24 a25 in
      let a22 = ( * ) a13 a2 in
      let a23 = ( * ) a9 a6 in
      let a17 = ( - ) a22 a23 in
      let a20 = ( * ) a9 a4 in
      let a21 = ( * ) a11 a2 in
      let a19 = ( - ) a20 a21 in
      (a15, a17, a19)
    let main =
      let a26 = (1, 2, 3) in
      let a27 = (4, 5, 6) in
      let a29 = cross a26 a27 in
      print_tuple a29
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
      let a12 = `list_is_empty list in
      let a1 = not a12 in
      if a1
      then
        let a3 = `list_hd list in
        let a5 = `list_tl list in
        let a7 = f a3 in
        let a8 = map f a5 in
        ( :: ) a7 a8
      else let a10 = `list_is_empty list in
        if a10
        then []
        else panic ()
    let map_ f_0 list_0 =
      let a22 = `list_is_empty list_0 in
      let a14 = not a22 in
      if a14
      then
        let a16 = `list_hd list_0 in
        let a18 = `list_tl list_0 in
        let a20 = f_0 a16 in
        let a21 = map f_0 a18 in
        ( :: ) a20 a21
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
    let __ll_helper_1 acc n_0 =
      let a31 = `get_tuple_field (n_0, acc) 0 in
      let a27 = ( = ) a31 0 in
      let a30 = `get_tuple_field (n_0, acc) 1 in
      let a29 = `list_is_empty a30 in
      let a28 = not a29 in
      let a1 = ( && ) a27 a28 in
      if a1
      then
        let a2 = (n_0, acc) in
        let a5 = `get_tuple_field a2 1 in
        let a4 = `list_hd a5 in
        a4
      else
        let a26 = `get_tuple_field (n_0, acc) 1 in
        let a25 = `list_is_empty a26 in
        let a20 = not a25 in
        let a24 = `get_tuple_field (n_0, acc) 1 in
        let a23 = `list_tl a24 in
        let a22 = `list_is_empty a23 in
        let a21 = not a22 in
        let a7 = ( && ) a20 a21 in
        if a7
        then
          let a8 = (n_0, acc) in
          let a19 = `get_tuple_field a8 1 in
          let a10 = `list_hd a19 in
          let a18 = `get_tuple_field a8 1 in
          let a17 = `list_tl a18 in
          let a12 = `list_hd a17 in
          let a16 = ( + ) a10 a12 in
          let a14 = ( :: ) a16 acc in
          let a15 = ( - ) n_0 1 in
          __ll_helper_1 a14 a15
        else -1
    let fib n = let a34 = ( - ) n 2 in
      __ll_helper_1 [1; 1] a34
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
    let add_cps x y k = let a1 = ( + ) x y in
      k a1
    let square_cps x_0 k_0 = let a3 = ( * ) x_0 x_0 in
      k_0 a3
    let __ll_4 k_1 x_squared y_squared = add_cps x_squared y_squared k_1
    let __ll_3 k_1 y_0 x_squared =
      let a6 = __ll_4 k_1 x_squared in
      square_cps y_0 a6
    let pythagoras_cps x_1 y_0 k_1 = let a8 = __ll_3 k_1 y_0 in
      square_cps x_1 a8
    |}]
;;

let%expect_test _ =
  test {|
    let f a0 a1 a2 a3 a4 a5 = a0 + a1 + a2 + a3 + a4 + a5
    |};
  [%expect {| |}]
;;

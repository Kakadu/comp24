(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Base
open Lib

let test_ll code =
  let open Format in
  let ll_ast =
    match Parser.parse_program code with
    | Error e -> failwith e
    | Ok ast ->
      (match Inferencer.inference_program ast with
       | Error e -> failwith (asprintf "%a" Pp_typing.pp_error e)
       | Ok ast ->
         ast
         |> Remove_patterns.remove_patterns
         |> Remove_match.remove_match
         |> Closure_conversion.closure_conversion
         |> Lambda_lifting.lambda_lifting)
  in
  printf
    "\n%s"
    (ll_ast |> List.map ~f:(asprintf "%a" Cf_ast.pp_definition) |> String.concat)
;;

let%expect_test _ =
  test_ll
    {|
    let _ = 
      let one = 1 in
      let t = true in
      let tup = (one, t) in
      let test = fun x -> x in
      ()
  |};
  [%expect {|
    let `test_4 x = x
    let _ =
      let one = 1 in
      let t = true in
      let tup = (one, t) in
      let test = `test_4 in
      ()
    |}]
;;

let%expect_test _ =
  test_ll "let a = (42, fun x->x)";
  [%expect {|
    let `ll_1 x = x
    let a = (42, `ll_1)
    |}]
;;

let%expect_test _ =
  test_ll {|
  let three = 
    let one = 1 in
    let two = 2 in
    one + two
  |};
  [%expect
    {|
    let three = let one = 1 in
      let two = 2 in
      (( + ) one two)
    |}]
;;

let%expect_test _ =
  test_ll
    {|
    let test x = 
      let one x = 1 in
      let two = 2 in
      let add_three x = x + two + (one ())
      in add_three x
    |};
  [%expect
    {|
    let `one_1 x = 1
    let `add_three_3 one two x = (( + ) (( + ) x two) (one ()))
    let test x =
      let one = `one_1 in
      let two = 2 in
      let add_three = `add_three_3 in
      (add_three one two x)
    |}]
;;

let%expect_test _ =
  test_ll
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
    let `one_1 x = 1
    let `add_three_4 one two x = (( + ) (( + ) x two) (one ()))
    let test x =
      let one = let z = 0 in
        `one_1 in
      let two = 2 in
      let add_three = `add_three_4 in
      (add_three one two x)
    |}]
;;

let%expect_test _ =
  test_ll {|
    let main k = 
      let add_k x y = (x + y) * k
      in add_k 1 2
    |};
  [%expect
    {|
    let `add_k_1 k x y = (( * ) (( + ) x y) k)
    let main k = let add_k = `add_k_1 in
      (add_k k 1 2)
    |}]
;;

let%expect_test _ =
  test_ll
    {|
    let main k = 
     let add_k x y = (x + y) * k in
     let waste_of_space = 0 in
     (42 + add_k 42 (-42))
    |};
  [%expect
    {|
    let `add_k_1 k x y = (( * ) (( + ) x y) k)
    let main k =
      let add_k = `add_k_1 in
      let waste_of_space = 0 in
      (( + ) 42 (add_k k 42 (-42)))
    |}]
;;

let%expect_test _ =
  test_ll
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
    let `ll_2 cont n res = (cont (( * ) n res))
    let `helper_1 n cont =
      if (( <= ) n 1)
      then (cont 1)
      else (`helper_1 (( - ) n 1) (`ll_2 cont n))
    let `ll_3 x = x
    let fact n = let helper = `helper_1 in
      (helper n `ll_3)
    |}]
;;

let%expect_test _ =
  test_ll
    {|
      let fact n =
        let rec helper acc n =
          if n <= 1 then 
            1
          else 
            helper (acc * n) (n - 1)
        in 
        helper 1 n
    |};
  [%expect
    {|
    let `helper_1 acc n =
      if (( <= ) n 1)
      then 1
      else (`helper_1 (( * ) acc n) (( - ) n 1))
    let fact n = let helper = `helper_1 in
      (helper 1 n)
    |}]
;;

let%expect_test _ =
  test_ll
    {|
    let gen seed1 seed2 = 
      let gen n = n * seed2 + seed1 * 42 in
      [gen 1; gen 2; gen 3]
    |};
  [%expect
    {|
    let `gen_1 seed1 seed2 n = (( + ) (( * ) n seed2) (( * ) seed1 42))
    let gen seed1 seed2 =
      let gen = `gen_1 in
      [(gen seed1 seed2 1); (gen seed1 seed2 2); (gen seed1 seed2 3)]
    |}]
;;

let%expect_test _ =
  test_ll
    {|
    let main x = 
      let const f s = f in
      let rev_const f s = const s in
      rev_const (fun s -> x)
    |};
  [%expect
    {|
    let `const_1 f s = f
    let `rev_const_2 const f s = (const s)
    let `ll_3 x s = x
    let main x =
      let const = `const_1 in
      let rev_const = `rev_const_2 in
      (rev_const const (`ll_3 x))
    |}]
;;

let%expect_test _ =
  test_ll
    {|
    let rec map f list = match list with
      | hd::tl -> f hd :: map f tl
      | _ -> []
    |};
  [%expect
    {|
    let map f list =
      if (( && ) (not (`list_is_empty list)) (( && ) true true))
      then
        let hd = (`list_hd list) in
        let tl = (`list_tl list) in
        (( :: ) (f hd) (map f tl))
      else []
    |}]
;;

let%expect_test _ =
  test_ll
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
    let add_cps x y k = (k (( + ) x y))
    let square_cps x k = (k (( * ) x x))
    let `ll_4 k x_squared y_squared = (add_cps x_squared y_squared k)
    let `ll_3 k y x_squared = (square_cps y (`ll_4 k x_squared))
    let pythagoras_cps x y k = (square_cps x (`ll_3 k y))
    |}]
;;

let%expect_test _ =
  test_ll
    {|
    let main = 
      let plus_one = 
        let one = 1 in 
        let func x = x + one in 
        func 
      in
      let x = plus_one 41 in
      print_int x 
    |};
  [%expect
    {|
    let `func_3 one x = (( + ) x one)
    let main =
      let plus_one = let one = 1 in
        let func = `func_3 in
        (func one) in
      let x = (plus_one 41) in
      (print_int x)
    |}]
;;

let%expect_test _ =
  test_ll
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
    let `helper_1 x acc n =
      if (( = ) n 0)
      then acc
      else let n = n in
        (`helper_1 x (( * ) acc x) (( - ) n 1))
    let pow x n = let helper = `helper_1 in
      (helper x 1 n)
    |}]
;;

let%expect_test _ =
  test_ll
    {|
    let pow n = match n with
      | 0 -> 0
      | n -> n * n
  |};
  [%expect {|
    let pow n = if (( = ) n 0)
      then 0
      else let n = n in
        (( * ) n n)
    |}]
;;

let%expect_test _ =
  test_ll {|
    let div x y =
      let x = x * 8 in
      x / y
  |};
  [%expect {|
    let div x y = let x = (( * ) x 8) in
      (( / ) x y)
    |}]
;;

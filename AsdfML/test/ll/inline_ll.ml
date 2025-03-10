(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Lib
open Test.Utils

let test_ll code =
  let open Format in
  lambda_lift code (printf "%a" Cf_ast.pp_program)
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
  [%expect
    {|
    let ll_test_4 x = x
    let _ =
      let one = 1 in
      let t = true in
      let tup = (one, t) in
      let test = ll_test_4 in
      ()
    |}]
;;

let%expect_test _ =
  test_ll "let a = (42, fun x->x)";
  [%expect {|
    let ll_1 x = x
    let a = (42, ll_1)
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
    let ll_one_1 x_0 = 1
    let ll_add_three_3 one two x_1 = (( + ) (( + ) x_1 two) (one ()))
    let test x =
      let one = ll_one_1 in
      let two = 2 in
      let add_three = ll_add_three_3 in
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
    let ll_one_1 x_0 = 1
    let ll_add_three_4 one two x_1 = (( + ) (( + ) x_1 two) (one ()))
    let test x =
      let one = let z = 0 in
        ll_one_1 in
      let two = 2 in
      let add_three = ll_add_three_4 in
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
    let ll_add_k_1 k x y = (( * ) (( + ) x y) k)
    let main k = let add_k = ll_add_k_1 in
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
    let ll_add_k_1 k x y = (( * ) (( + ) x y) k)
    let main k =
      let add_k = ll_add_k_1 in
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
    let ll_2 cont n_0 res = (cont (( * ) n_0 res))
    let ll_helper_1 n_0 cont =
      if (( <= ) n_0 1)
      then (cont 1)
      else (ll_helper_1 (( - ) n_0 1) (ll_2 cont n_0))
    let ll_3 x = x
    let fact n = let helper = ll_helper_1 in
      (helper n ll_3)
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
    let ll_helper_1 acc n_0 =
      if (( <= ) n_0 1)
      then 1
      else (ll_helper_1 (( * ) acc n_0) (( - ) n_0 1))
    let fact n = let helper = ll_helper_1 in
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
    let ll_gen_0_1 seed1 seed2 n = (( + ) (( * ) n seed2) (( * ) seed1 42))
    let gen seed1 seed2 =
      let gen_0 = ll_gen_0_1 in
      [(gen_0 seed1 seed2 1); (gen_0 seed1 seed2 2); (gen_0 seed1 seed2 3)]
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
    let ll_const_1 f s = f
    let ll_rev_const_2 const f_0 s_0 = (const s_0)
    let ll_3 x s_1 = x
    let main x =
      let const = ll_const_1 in
      let rev_const = ll_rev_const_2 in
      (rev_const const (ll_3 x))
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
      if (not (ml_list_is_empty list))
      then
        let hd = (ml_list_hd list) in
        let tl = (ml_list_tl list) in
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
    let square_cps x_0 k_0 = (k_0 (( * ) x_0 x_0))
    let ll_4 k_1 x_squared y_squared = (add_cps x_squared y_squared k_1)
    let ll_3 k_1 y_0 x_squared = (square_cps y_0 (ll_4 k_1 x_squared))
    let pythagoras_cps x_1 y_0 k_1 = (square_cps x_1 (ll_3 k_1 y_0))
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
    let ll_func_3 one x = (( + ) x one)
    let main =
      let plus_one = let one = 1 in
        let func = ll_func_3 in
        (func one) in
      let x_0 = (plus_one 41) in
      (print_int x_0)
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
  [%expect
    {|
    let ll_helper_1 x acc n_0 =
      if (( = ) n_0 0)
      then acc
      else let n_1 = n_0 in
        (ll_helper_1 x (( * ) acc x) (( - ) n_1 1))
    let pow x n = let helper = ll_helper_1 in
      (helper x 1 n)
    |}]
;;

let%expect_test _ =
  test_ll {|
    let pow n = match n with
      | 0 -> 0
      | n -> n * n
  |};
  [%expect
    {|
    let pow n = if (( = ) n 0)
      then 0
      else let n_0 = n in
        (( * ) n_0 n_0)
    |}]
;;

let%expect_test _ =
  test_ll {|
    let div x y =
      let x = x * 8 in
      x / y
  |};
  [%expect {|
    let div x y = let x_0 = (( * ) x 8) in
      (( / ) x_0 y)
    |}]
;;

let%expect_test _ =
  test_ll
    {|
    let f x = x
    let g = f
    let h = g
    let main = 
      let x = 42 in
      let fx = f x in
      let gx = g x in
      let hx = h x in
      ()
  |};
  [%expect
    {|
    let f x = x
    let g = f
    let h = g
    let main =
      let x_0 = 42 in
      let fx = (f x_0) in
      let gx = (g x_0) in
      let hx = (h x_0) in
      ()
    |}]
;;

let%expect_test _ =
  test_ll
    {|
  let ll_2 x = x
  let main =
    let one = 1 in
    let f1 = (fun x -> x + one) in
    let f2 = ll_2 in
    ()
  |};
  [%expect
    {|
    let __var_ll_2 x = x
    let ll_f1_3 one x_0 = (( + ) x_0 one)
    let main = let one = 1 in
      let f1 = ll_f1_3 in
      let f2 = __var_ll_2 in
      ()
    |}]
;;

let%expect_test _ =
  test_ll
    {|
  let main = 
      let () = println_int (( + ) 42 42) in
      let (+) a b = a - b in
      let () = println_int (( + ) 42 42) in
      let println_int x = println_int (x + 1) in
      let () = println_int ((+) 42 42) in
      ()
  |};
  [%expect
    {|
    let ll___ml_add_2 a b = (( - ) a b)
    let ll___ml_println_int_4 __ml_add x = (println_int (__ml_add x 1))
    let main =
      let () = (println_int (( + ) 42 42)) in
      let __ml_add = ll___ml_add_2 in
      let () = (println_int (__ml_add 42 42)) in
      let __ml_println_int = ll___ml_println_int_4 in
      let () = (__ml_println_int __ml_add (__ml_add 42 42)) in
      ()
    |}]
;;

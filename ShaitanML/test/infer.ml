(** Copyright 2024-2025, Nikita Lukonenko and Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Shaitanml_lib
open Infer

let%expect_test "function with narrowing type annotation" =
  test_infer
    {|
     let (f : int -> int) = fun x -> x;;
     |};
  [%expect
    {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val == : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val f : int -> int
    val print_int : int -> unit
    |}]
;;

let%expect_test "annotation for rec" =
  test_infer
    {|
     let x = let rec (f : int -> int) = fun x -> x in f 3;;
     |};
  [%expect
    {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val == : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val print_int : int -> unit
    val x : int
    |}]
;;

let%expect_test "list pattern in let" =
  test_infer
    {|
    let a::b = [1; 2; 3];;
     |};
  [%expect
    {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val == : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val a : int
    val b : int list
    val print_int : int -> unit
    |}]
;;

let%expect_test "nested annotation" =
  test_infer
    {|
    let f = let ((x : int):int) = 5 in x;;
     |};
  [%expect
    {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val == : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val f : int
    val print_int : int -> unit
    |}]
;;

let%expect_test "nested annotation unmatch" =
  test_infer
    {|
    let f ((x : int):int) (((y : int):string):int) = x;;
     |};
  [%expect
    {|
    Infer error: Unification failed on int and string |}]
;;

let%expect_test "simple annotation" =
  test_infer
    {|
    let (x:int) = 5;;
     |};
  [%expect
    {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val == : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val print_int : int -> unit
    val x : int
    |}]
;;

let%expect_test "fixed point combinator" =
  test_infer
    {|
      let rec fix f x = f (fix f) x ;;
     |};
  [%expect
    {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val == : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val fix : (('2 -> '3) -> '2 -> '3) -> '2 -> '3
    val print_int : int -> unit
    |}]
;;

let%expect_test "list folding" =
  test_infer
    {|
   let rec fold_left f acc l =
      match l with
      | [] -> acc
      | h :: tl -> fold_left f (f acc h) tl
   ;;
     |};
  [%expect
    {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val == : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val fold_left : ('4 -> '5 -> '4) -> '4 -> '5 list -> '4
    val print_int : int -> unit
    |}]
;;

let%expect_test "complex tuple" =
  test_infer
    {|
    let f x y = (x + y, [x; y])]
     |};
  [%expect
    {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val == : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val f : int -> int -> int * int list
    val print_int : int -> unit
    |}]
;;

let%expect_test "tuple of functions" =
  test_infer
    {|
      let fs = ((fun x -> x), (fun x y -> x + y))
     |};
  [%expect
    {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val == : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val fs : ('0 -> '0) * (int -> int -> int)
    val print_int : int -> unit
    |}]
;;

let%expect_test "unbound variable check" =
  test_infer
    {|
      let f x = x + y;;
      let y = 3;;
     |};
  [%expect {| Infer error: Unbound variable 'y' |}]
;;

let%expect_test "inner let expr" =
  test_infer
    {|
      let f x =
         let y = 3 in
         x + y;;
     |};
  [%expect
    {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val == : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val f : int -> int
    val print_int : int -> unit
    |}]
;;

let%expect_test "multiple args as functions" =
  test_infer
    {|
    let f a b c d e = a b c d e;;
     |};
  [%expect
    {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val == : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val f : ('1 -> '2 -> '3 -> '4 -> '5) -> '1 -> '2 -> '3 -> '4 -> '5
    val print_int : int -> unit
    |}]
;;

let%expect_test "cps function" =
  test_infer
    {|
      let map_cps f l =
        let rec helper k xs =
          match xs with
          | [] -> k []
          | h :: tl -> helper (fun r -> k ((f h) :: r)) tl
        in
        helper (fun x -> x) l
      ;;
     |};
  [%expect
    {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val == : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val map_cps : ('7 -> '9) -> '7 list -> '9 list
    val print_int : int -> unit
    |}]
;;

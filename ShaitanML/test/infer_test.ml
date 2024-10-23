(** Copyright 2023-2024, Nikita Lukonenko and Nikita Nemakin *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Shaitanml_lib
open Infer

let%expect_test _ =
  test_infer {|
    let f (x : 'a) (y : 'b) = x;;
     |};
  [%expect {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val x : int |}]
;;

let%expect_test _ =
  test_infer {|
      let rec fix f x = f (fix f) x ;;
     |};
  [%expect {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val fix : (('2 -> '3) -> '2 -> '3) -> '2 -> '3 |}]
;;

let%expect_test _ =
  test_infer
    {|
   let rec fold_left f acc l =
      match l with
      | [] -> acc
      | h :: tl -> fold_left f (f acc h) tl
   ;;
     |};
  [%expect {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val fold_left : ('11 -> '5 -> '11) -> '11 -> '5 list -> '11 |}]
;;

let%expect_test _ =
  test_infer {|
    let f x y = (x + y, [x; y])]
     |};
  [%expect {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val f : int -> int -> (int * int list) |}]
;;

let%expect_test _ =
  test_infer {|
      let fs = ((fun x -> x), (fun x y -> x + y))
     |};
  [%expect {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val fs : (('0 -> '0) * (int -> int -> int)) |}]
;;

let%expect_test _ =
  test_infer {|
      let f x = x + y;;
      let y = 3;;
     |};
  [%expect {| Infer error: Unbound variable 'y' |}]
;;

let%expect_test _ =
  test_infer {|
      let f x =
         let y = 3 in
         x + y;;
     |};
  [%expect {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val f : int -> int |}]
;;

let%expect_test _ =
  test_infer {|
    let f a b c d e = a b c d e;;
     |};
  [%expect {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val f : ('1 -> '2 -> '3 -> '4 -> '5) -> '1 -> '2 -> '3 -> '4 -> '5 |}]
;;

let%expect_test _ =
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
  [%expect {|
    val * : int -> int -> int
    val + : int -> int -> int
    val - : int -> int -> int
    val / : int -> int -> int
    val < : '1 -> '1 -> bool
    val <= : '1 -> '1 -> bool
    val <> : '1 -> '1 -> bool
    val = : '1 -> '1 -> bool
    val > : '1 -> '1 -> bool
    val >= : '1 -> '1 -> bool
    val map_cps : ('6 -> '8) -> '6 list -> '8 list |}]
;;

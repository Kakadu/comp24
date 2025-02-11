(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

(* Basic let declaration ([PVar] pattern) *)

let%expect_test _ =
  inference {| let f = 5;; let g = 6 |};
  [%expect {|
    val f : int
    val g : int |}]
;;

let%expect_test _ =
  inference {| let f x = x;; let g x = x;; let h = f g 0 |};
  [%expect {|
    val f : 'a -> 'a
    val g : 'a -> 'a
    val h : int |}]
;;

let%expect_test _ =
  inference {| let f x y = x + y;; let g x = x;; let h = f (g 0) 0 |};
  [%expect {|
    val f : int -> int -> int
    val g : 'a -> 'a
    val h : int |}]
;;

let%expect_test _ =
  inference {| let f (x, y) = x + y;; let g x = x;; let h = f ((g 0), 0) |};
  [%expect {|
    val f : int * int -> int
    val g : 'a -> 'a
    val h : int |}]
;;

let%expect_test _ =
  inference {| let f (x :: y :: []) = x + y;; let g x = x;; let h = f [(g 0); 0] |};
  [%expect {|
    val f : int list -> int
    val g : 'a -> 'a
    val h : int |}]
;;

(* ---------------- *)

(* Basic let declaration (complex patterns) *)

let%expect_test _ =
  inference {| let x, y = 1, 2;; let z :: w = [true; false] |};
  [%expect {|
    val x : int
    val y : int
    val z : bool
    val w : bool list |}]
;;

let%expect_test _ =
  inference {| let x, y = 1, 2;; let z :: w :: _ = true :: false :: [] |};
  [%expect {|
    val x : int
    val y : int
    val z : bool
    val w : bool |}]
;;

let%expect_test _ =
  inference {| let x, y = 1, 2;; let x :: y = [1; 2] |};
  [%expect {|
    val x : int
    val y : int list |}]
;;

let%expect_test _ =
  inference {| let _ = () |};
  [%expect {| |}]
;;

let%expect_test _ =
  inference {| let () = () |};
  [%expect {| |}]
;;

let%expect_test _ =
  inference {| let (((x, 2) :: []), (z, true)) = ([((), 2)], (0, true)) |};
  [%expect {|
    val x : unit
    val z : int |}]
;;

(* ---------------- *)

(* Basic let declaration with "and" ([PVar] pattern) *)

let%expect_test _ =
  inference {| let f x = x and g y = y |};
  [%expect {|
    val f : 'a -> 'a
    val g : 'a -> 'a |}]
;;


let%expect_test _ =
  inference {| let f x = x and g y = y + 1;; let res = (f true, f g 0) |};
  [%expect {|
    val f : 'a -> 'a
    val g : int -> int
    val res : bool * int |}]
;;

let%expect_test _ =
  inference {| let start = 0 and end = 5 and f x y = [x; y];; let res = f start end |};
  [%expect {|
    val start : int
    val end : int
    val f : 'a -> 'a -> 'a list
    val res : int list |}]
;;

(* ---------------- *)

(* Basic let declaration with "and" (complex patterns) *)

let%expect_test _ =
  inference {| let f x = x and (x, y) = (1, 2);; let res = (f x, f y) |};
  [%expect {|
    val f : 'a -> 'a
    val x : int
    val y : int
    val res : int * int |}]
;;

let%expect_test _ =
  inference {| let f x = x and (x, y, lst) = (1, 2, [3; 4; 5]);; let res = (f x, f y, lst) |};
  [%expect {|
    val f : 'a -> 'a
    val x : int
    val y : int
    val lst : int list
    val res : int * int * int list |}]
;;

let%expect_test _ =
  inference {| let () = () and f x = x and (x :: y :: []) = ((0, 1) :: (2, 3) :: []);; let res = (f x, f y) |};
  [%expect {|
    val f : 'a -> 'a
    val x : int * int
    val y : int * int
    val res : (int * int) * (int * int) |}]
;;

let%expect_test _ =
  inference {| let () = () and f x = x and (x :: f :: []) = ((0, 1) :: (2, 3) :: []);; let res = (f x, f y) |};
  [%expect {|
    Type error: variable 'f' is bound several times |}]
;;

(* ---------------- *)

(* Recursive let declaration *)

let%expect_test _ =
  inference {| let rec f x = f x |};
  [%expect {|
    val f : 'a -> 'b |}]
;;

let%expect_test _ =
  inference {| let rec f x = f (f x) |};
  [%expect {| val f : 'a -> 'a |}]
;;

let%expect_test _ =
  inference {| let rec f x = f |};
  [%expect {|
    Type error: occurs check failed. |}]
;;

let%expect_test _ =
  inference {| let rec f x = f (x :: []) |};
  [%expect {| Type error: occurs check failed. |}]
;;

let%expect_test _ =
  inference
    {| 
      let rec f x = if x = 1 then x else x * f (x-1)
    |};
  [%expect
    {|
    val f : int -> int |}]
;;

(* ---------------- *)

(* Let declaration with mutual recursion *)

let%expect_test _ =
  inference {|
    let rec f x = g x
    and g x = f x
  |};
  [%expect {|
    val f : 'a -> 'b
    val g : 'a -> 'b
    |}]
;;

let%expect_test _ =
  inference {|
    let rec f x = g (x + 1)
    and g x = f (x - 1)
  |};
  [%expect {|
    val f : int -> 'a
    val g : int -> 'a
    |}]
;;

let%expect_test _ =
  inference {|
    let rec f x = g x
    and g x = f (g x)
  |};
  [%expect {|
    val f : 'a -> 'a
    val g : 'a -> 'a
    |}]
;;

let%expect_test _ =
  inference {| 
    let rec is_even n = if n = 0 then true else is_odd (n - 1) 
    and is_odd n = if n = 0 then false else is_even (n - 1)

    let res1 = is_even 2
    let res2 = is_odd 2
  |};
  [%expect {|
    val is_even : int -> bool
    val is_odd : int -> bool
    val res1 : bool
    val res2 : bool
    |}]
;;

(* ---------------- *)

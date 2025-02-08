(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

(* Fun *)

let%expect_test _ =
  inference {| fun x -> x |};
  [%expect
    {| - : 'a -> 'a |}]
;;

let%expect_test _ =
  inference {| (fun x -> x) (fun x -> x) |};
  [%expect
    {| - : 'a -> 'a |}]
;;

let%expect_test _ =
  inference {| (fun x -> x) (true, false) |};
  [%expect
    {| - : bool * bool |}]
;;

let%expect_test _ =
  inference {| (fun x -> x + 1) |};
  [%expect
    {| - : int -> int |}]
;;

let%expect_test _ =
  inference {| (fun x -> x + 1) 5 |};
  [%expect
    {| - : int |}]
;;

let%expect_test _ =
  inference {| fun x -> fun y -> x + y |};
  [%expect
    {| - : int -> int -> int |}]
;;

let%expect_test _ =
  inference {| fun x -> fun y -> fun z -> x && y || y && z |};
  [%expect
    {| - : bool -> bool -> bool -> bool |}]
;;

let%expect_test _ =
  inference {| (fun x -> fun y -> fun z -> x && y || y && z) true false |};
  [%expect
    {| - : bool -> bool |}]
;;

let%expect_test _ =
  inference {| fun x -> fun y -> x |};
  [%expect
    {| - : 'a -> 'b -> 'a |}]
;;

let%expect_test _ =
  inference {| (fun x -> fun y -> y) (fun z -> z) |};
  [%expect
    {| - : 'a -> 'a |}]
;;

let%expect_test _ =
  inference {| (fun f -> f 42) (fun x -> x + 1) |};
  [%expect
    {| - : int |}]
;;

let%expect_test _ =
  inference {| (fun x -> y) |};
  [%expect
    {| Type error: unbound variable 'y' |}]
;;

(* ---------------- *)

(* Let in *)

let%expect_test _ =
  inference {| let f x = x in f |};
  [%expect
    {| - : 'a -> 'a |}]
;;

let%expect_test _ =
  inference {| let f x y = x + y in f 1 1 |};
  [%expect
    {| - : int |}]
;;

let%expect_test _ =
  inference {| let f x = x + x in f 10 |};
  [%expect
    {| - : int |}]
;;

let%expect_test _ =
  inference {| let f x = x * 2 in let g y = y + 3 in f (g 2) |};
  [%expect
    {| - : int |}]
;;

let%expect_test _ =
  inference {| let f x = x > 5 in f 4 |};
  [%expect
    {| - : bool |}]
;;

let%expect_test _ =
  inference {| let f x = y > 5 in f 4 |};
  [%expect
    {| Type error: unbound variable 'y' |}]
;;

(* ---------------- *)

(* Let in with and *)

let%expect_test _ =
  inference {| let f x = x and g x = x in (f, g) |};
  [%expect
    {| - : ('a -> 'a) * ('b -> 'b) |}]
;;

let%expect_test _ =
  inference {| let f x = x + 1 and g x = x * 2 in (f 2, g 3) |};
  [%expect
    {| - : int * int |}]
;;

let%expect_test _ =
  inference {| let f x = x > 0 and g x = x < 10 in f 5 && g 5 |};
  [%expect
    {| - : bool |}]
;;

(* ---------------- *)

(* Recursive let in *)

let%expect_test _ =
  inference {| let rec f x = f x in f |};
  [%expect
    {| - : 'a -> 'b |}]
;;

let%expect_test _ =
  inference {| let rec f x = if x = 0 then 1 else x * f (x - 1) in f 5 |};
  [%expect
    {| - : int |}]
;;

let%expect_test _ =
  inference {| let rec f x = if x then false else f true in f |};
  [%expect
    {| - : bool -> bool |}]
;;

let%expect_test _ =
  inference {| let rec f x = f (x + 1) in f 0 |};
  [%expect
    {| - : 'a |}]
;;

(* let%expect_test _ =
  inference {| let rec f x = f in f |};
  [%expect
    {| - : 'a -> 'b |}]
;; *)

(* ---------------- *)

(* Mutual recursive let in *)

let%expect_test _ =
  inference {| let rec f x = x and g x = f x in (f, g) |};
  [%expect
    {| - : ('a -> 'a) * ('b -> 'b) |}]
;;

let%expect_test _ =
  inference {| let rec even x = if x = 0 then true else odd (x - 1)
               and odd x = if x = 0 then false else even (x - 1) in even 4 |};
  [%expect
    {| - : bool |}]
;;

let%expect_test _ =
  inference {| let rec f x = g (x + 1) and g x = f (x - 1) in f 0 |};
  [%expect
    {| - : 'a |}]
;;

let%expect_test _ =
  inference {| let rec f x = x + g x and g x = x - f x in f 2 |};
  [%expect
    {| - : int |}]
;;

(* let%expect_test _ =
  inference {| let rec f x = g and g x = f in (f, g) |};
  [%expect
    {| - : ('a -> 'b) * ('c -> 'd -> 'b) |}]
;; *)

(* ---------------- *)
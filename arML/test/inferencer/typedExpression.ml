(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

(* Basic type *)

let%expect_test _ =
  inference_expression {|
    (1 : int)
  |};
  [%expect {| - : int |}]
;;

let%expect_test _ =
  inference_expression {|
    ("s" : string)
  |};
  [%expect {| - : string |}]
;;

let%expect_test _ =
  inference_expression {|
    (() : unit)
  |};
  [%expect {| - : unit |}]
;;

let%expect_test _ =
  inference_expression {|
    (true : bool)
  |};
  [%expect {| - : bool |}]
;;

let%expect_test _ =
  inference_expression {|
    ('c' : char)
  |};
  [%expect {| - : char |}]
;;

let%expect_test _ =
  inference_expression {|
    (true : 'a)
  |};
  [%expect {| - : bool |}]
;;

let%expect_test _ =
  inference_expression {|
    (1 : char)
  |};
  [%expect {| Type error: unification failed - type int does not match expected type char |}]
;;

let%expect_test _ =
  inference_expression {|
    (true : string)
  |};
  [%expect {| Type error: unification failed - type bool does not match expected type string |}]
;;

(* ---------------- *)

(* Tuples *)

let%expect_test _ =
  inference_expression {|
    ((0, 0) : int * int)
  |};
  [%expect {| - : int * int |}]
;;

let%expect_test _ =
  inference_expression {|
    ((0, true) : int * bool)
  |};
  [%expect {| - : int * bool |}]
;;

let%expect_test _ =
  inference_expression {|
    ((0, 0) : int * 'a)
  |};
  [%expect {| - : int * int |}]
;;

let%expect_test _ =
  inference_expression {|
    ((0, ((fun x -> x) : 'a -> 'a)) : int * 'a)
  |};
  [%expect {| - : int * ('a -> 'a) |}]
;;

(* ---------------- *)

(* Lists *)

let%expect_test _ =
  inference_expression {|
    ([] : int list)
  |};
  [%expect {| - : int list |}]
;;

let%expect_test _ =
  inference_expression {|
    (['a'; 'b'; ('c' : char)] : 'a list)
  |};
  [%expect {| - : char list |}]
;;

let%expect_test _ =
  inference_expression {|
    ([true; false] : int list)
  |};
  [%expect {| Type error: unification failed - type bool does not match expected type int |}]
;;

let%expect_test _ =
  inference_expression {|
    ((1 :: 2 :: []) : int list)
  |};
  [%expect {| - : int list |}]
;;

let%expect_test _ =
  inference_expression {|
    ((1 :: 2 :: []) : 'a list)
  |};
  [%expect {| - : int list |}]
;;

let%expect_test _ =
  inference_expression {|
    ((1 :: (2 : int list)) : 'a list)
  |};
  [%expect {| Type error: unification failed - type int does not match expected type int list |}]
;;

let%expect_test _ =
  inference_expression {|
    ((1 :: ([2] : int list)) : 'a list)
  |};
  [%expect {| - : int list |}]
;;

(* ---------------- *)

(* Funcations *)

let%expect_test _ =
  inference_expression {|
    fun (x : int) -> x
  |};
  [%expect {| - : int -> int |}]
;;

let%expect_test _ =
  inference_expression {|
    fun (x : int) : bool -> x
  |};
  [%expect {| Type error: unification failed - type int does not match expected type bool |}]
;;

let%expect_test _ =
  inference_expression {|
    fun ((x, y) : int * bool) : ('a -> int) -> fun _ -> x
  |};
  [%expect {| - : int * bool -> 'a -> int |}]
;;

let%expect_test _ =
  inference_expression {|
    let f = (1 : int) in f
  |};
  [%expect {| - : int |}]
;;

let%expect_test _ =
  inference_expression {|
    let f = 1 in (f : int)
  |};
  [%expect {| - : int |}]
;;

let%expect_test _ =
  inference_expression {|
    let f = 1 in (f : bool)
  |};
  [%expect {| Type error: unification failed - type int does not match expected type bool |}]
;;

let%expect_test _ =
  inference_expression {|
    let f (x : int) = x in f
  |};
  [%expect {| - : int -> int |}]
;;

let%expect_test _ =
  inference_expression {|
    let f (x : int) : int = x in f
  |};
  [%expect {| - : int -> int |}]
;;

let%expect_test _ =
  inference_expression {|
    let f x y : (int list * bool list) = ([x; y], []) in f
  |};
  [%expect {| - : int -> int -> int list * bool list |}]
;;

let%expect_test _ =
  inference_expression {|
    let f x = x and (x, y) : ('a * 'a) = (true, false) in (f x, f y)
  |};
  [%expect {| - : bool * bool |}]
;;

let%expect_test _ =
  inference_expression {|
    let f x : int = x and (x, y) : ('a * 'a) = (true, false) in (f x, f y)
  |};
  [%expect {| Type error: unification failed - type int does not match expected type bool |}]
;;

(* ---------------- *)

(* Declarations *)

let%expect_test _ =
  inference_expression {|
    let identity (x : 'a) : 'a = x
  |};
  [%expect {| val identity : 'a -> 'a |}]
;;

let%expect_test _ =
  inference_expression {|
    let f x : int = x
  |};
  [%expect {| val f : int -> int |}]
;;

let%expect_test _ =
  inference_expression {|
    let apply (f : 'a -> 'b) (x : 'a) : 'b = f x
  |};
  [%expect {| val apply : ('a -> 'b) -> 'a -> 'b |}]
;;

let%expect_test _ =
  inference_expression {|
    let f x : int =
      match x with
      | 0 -> 0
      | _ -> 1
  |};
  [%expect {| val f : int -> int |}]
;;

let%expect_test _ =
  inference_expression {|
    let rec factorial (n : int) : int =
      if n = 0 then 1
      else n * (factorial (n - 1) : int)
  |};
  [%expect {| val factorial : int -> int |}]
;;

let%expect_test _ =
  inference_expression {|
    let rec even (x : int) : bool =
      match x with
      | 0 -> true
      | _ -> odd (x - 1)
    and odd (x : int) : bool =
      match x with
      | 0 -> false
      | _ -> even (x - 1)
  |};
  [%expect {|
    val even : int -> bool
    val odd : int -> bool |}]
;;

let%expect_test _ =
  inference_expression {|
    let rec even (x : int) : 'a =
      match x with
      | 0 -> true
      | _ -> odd (x - 1)
    and odd (x : bool) : 'a =
      match x with
      | 0 -> false
      | _ -> even (x - 1)
  |};
  [%expect {|
    Type error: unification failed - type bool does not match expected type int |}]
;;

(* ---------------- *)

(* Pattern matching *)

let%expect_test _ =
  inference_expression {|
    fun x -> match (x : 'a * 'b) with | (0, 1) -> true | _ -> false
  |};
  [%expect {|
    - : int * int -> bool |}]
;;

let%expect_test _ =
  inference_expression {|
    fun x -> match (x : 'a * 'b) with | (0, 1) -> true | (true, false) -> false
  |};
  [%expect {|
    Type error: unification failed - type bool does not match expected type int |}]
;;

let%expect_test _ =
  inference_expression {|
    fun x -> match (x : (int list * 'a)) with | (x :: y, true) -> (x, y, true)  | (x :: y, false) -> (x, y, false)
  |};
  [%expect {|
    - : int list * bool -> int * int list * bool |}]
;;

(* ---------------- *)

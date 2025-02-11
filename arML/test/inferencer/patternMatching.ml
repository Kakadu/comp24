(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

(* See declarations with pattern matching tests in declaration.ml *)
(* See "let in" with pattern matching tests in function.ml *)
(* See "function" expr with pattern matching tests in function.ml *)

let%expect_test _ =
  inference {| fun x -> match x with
    | 0 -> "zero"
    | 1 -> "one"
    | _ -> "many"
  |};
  [%expect {| - : int -> string |}]
;;

let%expect_test _ =
  inference {| fun x y -> match (x, y) with
    | (0, y) -> y
    | (x, 0) -> x
    | (x, y) -> x + y
  |};
  [%expect {| - : int -> int -> int |}]
;;

let%expect_test _ =
  inference {| fun xs -> match xs with
    | [] -> "empty"
    | [x] -> "one element"
    | _ :: _ -> "multiple"
  |};
  [%expect {| - : 'a list -> string |}]
;;

let%expect_test _ =
  inference {| fun x y -> match (x, y) with
    | (1, 1) -> "both one"
    | (0, 0) -> "both zero"
    | _ -> "other"
  |};
  [%expect {| - : int -> int -> string |}]
;;

let%expect_test _ =
  inference {| fun xs -> match xs with
    | [] -> "empty list"
    | x :: xs -> "non-empty list"
  |};
  [%expect {| - : 'a list -> string |}]
;;

let%expect_test _ =
  inference {| fun x y -> match (x, y) with
    | (a, b) -> a + b
  |};
  [%expect {| - : int -> int -> int |}]
;;

let%expect_test _ =
  inference {| fun x y -> match (x, y) with
    | (a, b) -> a + b
    | _ -> true
  |};
  [%expect {| Type error: unification failed - type bool does not match expected type int |}]
;;

let%expect_test _ =
  inference {| fun x y -> match (x, y) with
    | (a, b) -> a + b
    | (a, b, c) -> a + b + c
  |};
  [%expect {| Type error: unification failed - type 'a * 'b does not match expected type 'a * 'b * 'c |}]
;;

let%expect_test _ =
  inference {| fun x y ->  match (x, y) with
    | (a, b, (k :: z)) -> a + b + k
    | _ -> 0
  |};
  [%expect {| Type error: unification failed - type 'a * 'b does not match expected type 'a * 'b * 'c list |}]
;;

let%expect_test _ =
  inference {| fun x y z ->  match (x, y, z) with
    | (a, b, (k :: z)) -> a + b + k
    | _ -> 0
  |};
  [%expect {| - : int -> int -> int list -> int |}]
;;

let%expect_test _ =
  inference {| fun x y z ->  match (x, y, z) with
    | (a, b, (k :: z)) -> a + b + k + z
    | _ -> 0
  |};
  [%expect {| Type error: unification failed - type int does not match expected type int list |}]
;;

let%expect_test _ =
  inference {| fun x y -> match (x, y) with
    | (a, b, (k :: a)) -> a + b + k
    | _ -> 0
  |};
  [%expect {| Type error: variable 'a' is bound several times |}]
;;

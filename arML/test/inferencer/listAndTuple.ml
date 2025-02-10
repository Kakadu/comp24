(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

(* Lists *)

let%expect_test _ =
  inference {| 1 :: 2 :: [] |};
  [%expect {| - : int list |}]
;;

let%expect_test _ =
  inference {| [1; 2; 3] |};
  [%expect {| - : int list |}]
;;

let%expect_test _ =
  inference {| [1; "a"] |};
  [%expect {| Type error: unification failed - type string does not match expected type int |}]
;;

(* ---------------- *)

(* Tuples *)

let%expect_test _ =
  inference {| (1, 2) |};
  [%expect {| - : int * int |}]
;;

let%expect_test _ =
  inference {| (0, (), 0) |};
  [%expect {| - : int * unit * int |}]
;;

let%expect_test _ =
  inference {| (fun x -> x), (fun y x -> x y), (let f x = x + 1 in f 0) |};
  [%expect {| - : ('a -> 'a) * ('b -> ('b -> 'c) -> 'c) * int |}]
;;

(* ---------------- *)

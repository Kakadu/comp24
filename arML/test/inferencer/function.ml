(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open ArML_lib.Runner

(* Fun expression ([PVar] pattern) *)

let%expect_test _ =
  inference {| fun x -> x |};
  [%expect {| - : 'a -> 'a |}]
;;

let%expect_test _ =
  inference {| (fun x -> x) (fun x -> x) |};
  [%expect {| - : 'a -> 'a |}]
;;

let%expect_test _ =
  inference {| (fun x -> x) (true, false) |};
  [%expect {| - : bool * bool |}]
;;

let%expect_test _ =
  inference {| (fun x -> x + 1) |};
  [%expect {| - : int -> int |}]
;;

let%expect_test _ =
  inference {| (fun x -> x + 1) 5 |};
  [%expect {| - : int |}]
;;

let%expect_test _ =
  inference {| fun x -> fun y -> x + y |};
  [%expect {| - : int -> int -> int |}]
;;

let%expect_test _ =
  inference {| fun x -> fun y -> fun z -> x && y || y && z |};
  [%expect {| - : bool -> bool -> bool -> bool |}]
;;

let%expect_test _ =
  inference {| (fun x -> fun y -> fun z -> x && y || y && z) true false |};
  [%expect {| - : bool -> bool |}]
;;

let%expect_test _ =
  inference {| fun x -> fun y -> x |};
  [%expect {| - : 'a -> 'b -> 'a |}]
;;

let%expect_test _ =
  inference {| (fun x -> fun y -> y) (fun z -> z) |};
  [%expect {| - : 'a -> 'a |}]
;;

let%expect_test _ =
  inference {| (fun f -> f 42) (fun x -> x + 1) |};
  [%expect {| - : int |}]
;;

let%expect_test _ =
  inference {| (fun x -> y) |};
  [%expect {| Type error: unbound variable 'y' |}]
;;

let%expect_test _ =
  inference {| fun x -> if x > 0 then x > 0 else x < 0 |};
  [%expect {| - : int -> bool |}]
;;

(* ---------------- *)

(* Fun expression (compex patterns) *)

let%expect_test _ =
  inference {| fun _ -> () |};
  [%expect {| - : 'a -> unit |}]
;;

let%expect_test _ =
  inference {| fun () -> () |};
  [%expect {| - : unit -> unit |}]
;;

let%expect_test _ =
  inference {| fun (x :: y) -> x :: y |};
  [%expect {| - : 'a list -> 'a list |}]
;;

let%expect_test _ =
  inference {| fun (x, y) -> x + y |};
  [%expect {| - : int * int -> int |}]
;;

let%expect_test _ =
  inference {| fun ((x, y), (z, w)) -> x + y + z + w |};
  [%expect {| - : (int * int) * (int * int) -> int |}]
;;

let%expect_test _ =
  inference {| fun ((x, y), (z, w)) -> x (y z) w |};
  [%expect {| - : (('a -> 'b -> 'c) * ('d -> 'a)) * ('d * 'b) -> 'c |}]
;;

let%expect_test _ =
  inference {| fun ((x, y), (z, w)) -> (x, y, z, w) |};
  [%expect {| - : ('a * 'b) * ('c * 'd) -> 'a * 'b * 'c * 'd |}]
;;

let%expect_test _ =
  inference {| fun ((x, y), z) -> x + y + z |};
  [%expect {| - : (int * int) * int -> int |}]
;;

let%expect_test _ =
  inference {| fun (x, (y, z, x)) -> x + y + z |};
  [%expect {| Type error: variable 'x' is bound several times |}]
;;

let%expect_test _ =
  inference {| fun (x :: y :: (a :: (b, y) :: [])) -> x + y + z |};
  [%expect {| Type error: variable 'y' is bound several times |}]
;;

(* ---------------- *)

(* Let in ([PVar] pattern) *)

let%expect_test _ =
  inference {| let f x = x in f |};
  [%expect {| - : 'a -> 'a |}]
;;

let%expect_test _ =
  inference {| let f x y = x + y in f 1 1 |};
  [%expect {| - : int |}]
;;

let%expect_test _ =
  inference {| let f x = x + x in f 10 |};
  [%expect {| - : int |}]
;;

let%expect_test _ =
  inference {| let f x = x * 2 in let g y = y + 3 in f (g 2) |};
  [%expect {| - : int |}]
;;

let%expect_test _ =
  inference {| let f x = x > 5 in f 4 |};
  [%expect {| - : bool |}]
;;

let%expect_test _ =
  inference {| let f x = y > 5 in f 4 |};
  [%expect {| Type error: unbound variable 'y' |}]
;;

let%expect_test _ =
  inference {| let f x = x + 1 in f "a" |};
  [%expect {| Type error: unification failed - type int does not match expected type string |}]
;;

(* ---------------- *)

(* Let in (complex patterns) *)

let%expect_test _ =
  inference {| let x, y = (0, 0) in (x, y) |};
  [%expect {| - : int * int |}]
;;

let%expect_test _ =
  inference {| let x :: y = [1 ; 2] in (x :: y) |};
  [%expect {| - : int list |}]
;;

let%expect_test _ =
  inference {| let x, y = ((fun x -> x + 1), 0) in x y |};
  [%expect {| - : int |}]
;;

let%expect_test _ =
  inference {| let _ = () in 0 |};
  [%expect {| - : int |}]
;;

let%expect_test _ =
  inference {| let (x, y) = 1 in (x :: y) |};
  [%expect {| Type error: unification failed - type 'a * 'b does not match expected type int |}]
;;

let%expect_test _ =
  inference {| let (x, y) = ((fun x -> x + 1), true) in x y |};
  [%expect {| Type error: unification failed - type int does not match expected type bool |}]
;;

(* ---------------- *)

(* Let in with and *)

let%expect_test _ =
  inference {| let f x = x and g x = x in (f, g) |};
  [%expect {| - : ('a -> 'a) * ('b -> 'b) |}]
;;

let%expect_test _ =
  inference {| let f x = x + 1 and g x = x * 2 in (f 2, g 3) |};
  [%expect {| - : int * int |}]
;;

let%expect_test _ =
  inference {| let f x = x > 0 and g x = x < 10 in f 5 && g 5 |};
  [%expect {| - : bool |}]
;;

let%expect_test _ =
  inference {| let (x, y) = (true, false) and g x y = true && false in g x y |};
  [%expect {| - : bool |}]
;;

let%expect_test _ =
  inference {| let (x, y) = (true, false) and g x y = true && false and (k :: z) = [1; 2] in (g x y, k, z) |};
  [%expect {| - : bool * int * int list |}]
;;

let%expect_test _ =
  inference {| let (x, y) = (true, false) and g x y = true && false and (x :: y) = [1; 2] in g x y |};
  [%expect {| Type error: variable 'x' is bound several times |}]
;;

(* ---------------- *)

(* Recursive let in *)

let%expect_test _ =
  inference {| let rec f x = f x in f |};
  [%expect {| - : 'a -> 'b |}]
;;

let%expect_test _ =
  inference {| let rec f x = f (f x) in f |};
  [%expect {| - : 'a -> 'a |}]
;;

let%expect_test _ =
  inference {| let rec f x = if x = 0 then 1 else x * f (x - 1) in f 5 |};
  [%expect {| - : int |}]
;;

let%expect_test _ =
  inference {| let rec f x = if x then false else f true in f |};
  [%expect {| - : bool -> bool |}]
;;

let%expect_test _ =
  inference {| let rec f x = if x then 0 else (f true) && true in f |};
  [%expect {| Type error: unification failed - type int does not match expected type bool |}]
;;

let%expect_test _ =
  inference {| let rec f x = f (x + 1) in f 0 |};
  [%expect {| - : 'a |}]
;;

let%expect_test _ =
  inference {| let rec f x = f in f |};
  [%expect {| Type error: occurs check failed. |}]
;;

let%expect_test _ =
  inference {| let rec (x, y) = (1, 2) |};
  [%expect {| Only variables are allowed as left-hand side of `let rec' |}]
;;

(* ---------------- *)

(* Mutual recursive let in *)

let%expect_test _ =
  inference {| let rec f x = x and g x = f x in (f, g) |};
  [%expect {| - : ('a -> 'a) * ('b -> 'b) |}]
;;

let%expect_test _ =
  inference
    {| let rec even x = if x = 0 then true else odd (x - 1)
               and odd x = if x = 0 then false else even (x - 1) in even 4 |};
  [%expect {| - : bool |}]
;;

let%expect_test _ =
  inference {| let rec f x = g (x + 1) and g x = f (x - 1) in f 0 |};
  [%expect {| - : 'a |}]
;;

let%expect_test _ =
  inference {| let rec f x = x + g x and g x = x - f x in f 2 |};
  [%expect {| - : int |}]
;;

let%expect_test _ =
  inference {| let rec f x = x + 1 and g x = x - 1 in (f 3, g 3) |};
  [%expect {| - : int * int |}]
;;

let%expect_test _ =
  inference {| let rec f x = g and g x = f in (f, g) |};
  [%expect {| Type error: occurs check failed. |}]
;;

let%expect_test _ =
  inference {| let rec f x = f x and (y :: z) = (1 :: []) in f (y :: z) |};
  [%expect {| Only variables are allowed as left-hand side of `let rec' |}]
;;

(* ---------------- *)

(* Function expression *)

let%expect_test _ =
  inference {| function x -> x * 2 |};
  [%expect {| - : int -> int |}]
;;

let%expect_test _ =
  inference {| function (x, y) -> x + y |};
  [%expect {| - : int * int -> int |}]
;;

let%expect_test _ =
  inference {| function
    | 0 -> "zero"
    | 1 -> "one"
    | _ -> "many"
  |};
  [%expect {| - : int -> string |}]
;;

let%expect_test _ =
  inference {| function
    | [] -> "empty"
    | [x] -> "one element"
    | _ :: _ -> "multiple"
  |};
  [%expect {| - : 'a list -> string |}]
;;

let%expect_test _ =
  inference {| function
    | (0, y) -> y
    | (x, 0) -> x
    | (x, y) -> x + y
  |};
  [%expect {| - : int * int -> int |}]
;;

let%expect_test _ =
  inference {| function
    | [] -> 0
    | [x] -> x
    | x1 :: x2 :: xs -> x1 + x2
  |};
  [%expect {| - : int list -> int |}]
;;

let%expect_test _ =
  inference {| function
    | (x, (0, z)) -> x + z
    | (x, (y, x)) -> x + y
  |};
  [%expect {| Type error: variable 'x' is bound several times |}]
;;

(* ---------------- *)

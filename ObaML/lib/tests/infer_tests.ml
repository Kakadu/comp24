(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open ObaML
open Format


(***************************Structure*Infer*Tests***************************)

let parse_and_infer_result str =
  match Parser.structure_from_string str with
  | Ok parse_result ->
    (match Inferencer.run_stucture_infer parse_result with
     | Ok env -> printf "%a" Inferencer.TypeEnv.pretty_pp_env env
     | Error err -> printf "%a" Typedtree.pp_error err)
  | Error _ -> printf "Syntax error"
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| 2 + 2 |} in
  [%expect {| |}]
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| let a = 5 |} in
  [%expect {|
    val a : int|}]
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| let (a, b) = 5, "s" |} in
  [%expect {|
    val a : int
    val b : string|}]
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| let n = fun (a, b) -> a + 1 |} in
  [%expect {|
      val n : int * 'a -> int |}]
;;

let%expect_test "" =
  let _ = parse_and_infer_result {|let (a :: b) = 1 :: 1 :: [] |} in
  [%expect {|
    val a : int
    val b : int list |}]
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| let n a b = a + 1, b |} in
  [%expect {|
      val n : int -> 'a -> int * 'a |}]
;;

let%expect_test _ =
  let _ = parse_and_infer_result {| let (a, b, _) = 1, 2, 3 and c = "s" |} in
  [%expect
    {|
      val a : int
      val b : int
      val c : string |}]
;;

let%expect_test _ =
  let _ =
    parse_and_infer_result
      {| let n = fun x -> match x with | true -> true | false -> false |}
  in
  [%expect {|
      val n : bool -> bool |}]
;;

let%expect_test "" =
  parse_and_infer_result
    {| let rec is_even n =
    if n = 0 then true
    else is_odd (n - 1)
  
  and is_odd n =
    if n = 0 then false
    else is_even (n - 1);;  |};
  [%expect {|
    val is_even : int -> bool
    val is_odd : int -> bool |}]
;;

let%expect_test _ =
  parse_and_infer_result
    {| let rec fix = fun f -> (fun x -> f (fix f) x)
           let fac = fix (fun self -> (fun n -> if n <= 1 then 1 else n * self (n - 1)))
           let a = fac 5 |};
  [%expect
    {|
        val a : int
        val fac : int -> int
        val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let n = 5 |};
  [%expect {|
    val n : int |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let n = (5, 4, "a") |};
  [%expect {|
    val n : int * int * string |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let n = 5 :: [] 
  let b = n|};
  [%expect {|
    val b : int list
    val n : int list |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let b = [1; 2] |};
  [%expect {|
      val b : int list |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let b = [1 :: []] |};
  [%expect {|
      val b : int list list |}]
;;

let%expect_test _ =
  parse_and_infer_result
    {| let b = []
         let a = "a"
         let f = 5 
         let id = f  |};
  [%expect {|
    val a : string
    val b : 'a list
    val f : int
    val id : int |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let n = fun x -> x + 1 
  let b = n 5|};
  [%expect {|
    val b : int
    val n : int -> int |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let n = fun x -> x
  let b = n 5
  let c = n true |};
  [%expect {|
    val b : int
    val c : bool
    val n : 'a -> 'a |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let n = fun a :: b -> a 
  let b = n (46 :: 52 :: []) |};
  [%expect {|
      val b : int
      val n : 'a list -> 'a |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let n = fun (a, b) -> a + 1 |};
  [%expect {|
      val n : int * 'a -> int |}]
;;

let%expect_test _ =
  parse_and_infer_result
    {| let n = fun x -> match x with | true -> true | false -> false |};
  [%expect {|
      val n : bool -> bool |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let x = if true then if true then 1 else 2 else 3 |};
  [%expect {|
      val x : int |}]
;;

let%expect_test _ =
  parse_and_infer_result
    {| let rec factorial_recursive = fun n -> if n <= 1 then 1 else n * factorial_recursive (n - 1)
    let a = factorial_recursive 5
    let b = factorial_recursive 6 |};
  [%expect
    {|
      val a : int
      val b : int
      val factorial_recursive : int -> int |}]
;;

let%expect_test _ =
  parse_and_infer_result
    {| let rec fix = fun f -> (fun x -> f (fix f) x)
           let fac = fix (fun self -> (fun n -> if n <= 1 then 1 else n * self (n - 1)))
           let a = fac 5 |};
  [%expect
    {|
      val a : int
      val fac : int -> int
      val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let rec fix = fun f -> (fun x -> f (fix f) x) |};
  [%expect {|
      val fix : (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b |}]
;;

let%expect_test _ =
  parse_and_infer_result
    {| let rec f = fun (a, b) -> if a + b < 10 then a + b else f (a-1,b-1) |};
  [%expect {|
      val f : int * int -> int |}]
;;

let%expect_test _ =
  parse_and_infer_result
    {|
      let rev = fun lst ->
        (let rec helper = fun acc -> (fun lst ->
        match lst with
          | [] -> acc
          | h :: tl -> helper (h :: acc) tl)
        in
        helper [] lst)
      let reversed1 = rev (1 :: 2 :: 3 :: 4 :: 5 :: [])
      let reversed2 = rev (true :: false :: false :: false :: [])
    |};
  [%expect
    {|
      val rev : 'a list -> 'a list
      val reversed1 : int list
      val reversed2 : bool list |}]
;;

let%expect_test _ =
  parse_and_infer_result {| let a = let b x = x + 1 in b;;
    let c = a 4;;|};
  [%expect {|
      val a : int -> int
      val c : int |}]
;;

let%expect_test _ =
  parse_and_infer_result
    {| let x = let f a b = let inc = (fun a -> a + 1) in (fun b -> b) inc (a b) in f |};
  [%expect {|
      val x : ('a -> int) -> 'a -> int |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let map f p = let (a,b) = p in (f a, f b) |};
  [%expect {|
    val map : ('a -> 'b) -> 'a * 'a -> 'b * 'b |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let a b (c, d) = (b c, b d) |};
  [%expect {|
    val a : ('a -> 'b) -> 'a * 'a -> 'b * 'b |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let map p = let (a,b) = p in a + b |};
  [%expect {|
        val map : int * int -> int |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let map = fun p -> let a = p in a |};
  [%expect {|
        val map : 'a -> 'a|}]
;;

let%expect_test "" =
  parse_and_infer_result {| let rec map = fun p -> let rec a = p in a |};
  [%expect {|
        val map : 'a -> 'a|}]
;;

let%expect_test _ =
  parse_and_infer_result {| let a (x: int) = x |};
  [%expect {|
      val a : int -> int |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let a (b: bool * string) (c: int -> int): int = 5;; |};
  [%expect {|
    val a : bool * string -> (int -> int) -> int
     |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let a b c: int = 5;; |};
  [%expect {|
    val a : 'a -> 'b -> int
     |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let a b : int -> int = fun x -> b x;; |};
  [%expect {|
    val a : (int -> int) -> int -> int
     |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let a b x: int -> int = b x;; |};
  [%expect {|
    val a : ('a -> int -> int) -> 'a -> int -> int
     |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let a b (x: int): int -> int = b x;; |};
  [%expect {|
    val a : (int -> int -> int) -> int -> int -> int
     |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let a b = b;; |};
  [%expect {|
    val a : 'a -> 'a
     |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let a b : int = b;; |};
  [%expect {|
    val a : int -> int
     |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let (a, a) = (5, 4);; |};
  [%expect {|
    Variable "a" is bound several times in this matching
     |}]
;;

let%expect_test "" =
  parse_and_infer_result {| let ( + ) a = a;;
  let b = ( + ) "s" |};
  [%expect {|
    val ( + ) : 'a -> 'a
    val b : string
     |}]
;;

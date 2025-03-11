[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Stdio

let run s =
  match LParse.parse s with
  | None ->
      print_endline "syntax error"
  | Some str ->
      PPrint.ToChannel.pretty 1. 40 stdout (LPrint.pp_structure str)

let%expect_test _ =
  run {| let rec fact n = if n <= 1 then 1 else n * fact (n - 1) |} ;
  [%expect
    {|
    let rec fact =
      fun n ->
        if ( <= ) n 1
        then 1
        else ( * ) n (fact (( - ) n 1))
    |}]

(* ======= Patterns ======= *)

let%expect_test _ =
  run {| let Cons (hd, tl) = () |} ;
  [%expect {| let Cons (hd, tl) = () |}]

let%expect_test _ =
  run {| let C _ | a, b = () |} ;
  [%expect {| let C _ | a, b = () |}]

let%expect_test _ =
  run {| let a | (b | c) | d = () |} ;
  [%expect {| let a | (b | c) | d = () |}]

let%expect_test _ =
  run {| let a, (b, c), d = () |} ;
  [%expect {| let a, (b, c), d = () |}]

let%expect_test _ =
  run {| let a, b | c, d = () |} ;
  [%expect {| let a, b | c, d = () |}]

let%expect_test _ =
  run {| let a::(b::c)::d = () |} ;
  [%expect {| let a :: (b :: c) :: d = () |}]

let%expect_test _ =
  run {| let a::b::c,d|e = () |} ;
  [%expect {| let a :: b :: c, d | e = () |}]

let%expect_test _ =
  run {| let [a;b;c] = () |} ; [%expect {| let a :: b :: c :: [] = () |}]

let%expect_test _ = run {| let [a] = () |} ; [%expect {| let a :: [] = () |}]

let%expect_test _ = run {| let [] = () |} ; [%expect {| let [] = () |}]

let%expect_test _ =
  run {| let hd1::hd2::tl = () |} ;
  [%expect {| let hd1 :: hd2 :: tl = () |}]

let%expect_test _ =
  run {| let ( x : int ) = 1 |} ;
  [%expect {| let (x : int) = 1 |}]

let%expect_test _ =
  run {| let Some Some (x : int) = Some (Some 1) |} ;
  [%expect {| let Some Some (x : int) = Some (Some 1) |}]

let%expect_test _ =
  run {| let Some Some x : int option option = Some (Some 1) |} ;
  [%expect
    {|
    let (Some Some x : (int option) option) =
      Some (Some 1)
    |}]

(* ======= Expressions ======= *)

let%expect_test _ =
  run {| function | a -> true | b -> false |} ;
  [%expect {| function a -> true | b -> false |}]

let%expect_test _ =
  run {| fun x y -> x + y |} ; [%expect {| fun x y -> ( + ) x y |}]

let%expect_test _ = run {| a0b'c_d |} ; [%expect {| a0b'c_d |}]

let%expect_test _ =
  run "a >>= b ++ c ** d !+ e" ;
  [%expect {|
    ( >>= ) a
      (( ++ ) b (( ** ) c (d (( !+ ) e))))
    |}]

let%expect_test _ =
  run {| let rec a = 1 and b = 2 in let e = 3 in a |} ;
  [%expect {|
    let rec a = 1 and b = 2 in
    let e = 3 in a
    |}]

let%expect_test _ =
  run {| if a then (if b then c) else d |} ;
  [%expect {| if a then if b then c else d |}]

let%expect_test _ =
  run {| if a; b then c; d |} ;
  [%expect {| if a; b then c; d |}]

let%expect_test _ =
  run {| if a; b then (c; d) |} ;
  [%expect {| if a; b then c; d |}]

let%expect_test _ =
  run {| match a with b -> c | d -> e |} ;
  [%expect {| match a with b -> c | d -> e |}]

let%expect_test _ =
  run {| match a with | b | c | d -> e | f -> g |} ;
  [%expect {| match a with b | c | d -> e | f -> g |}]

let%expect_test _ = run {| Nil |} ; [%expect {| Nil |}]

let%expect_test _ = run {| Some x |} ; [%expect {| Some x |}]

let%expect_test _ = run {| Cons (1, Nil) |} ; [%expect {| Cons (1, Nil) |}]

let%expect_test _ = run {| [a;b;c] |} ; [%expect {| a :: b :: c :: [] |}]

let%expect_test _ = run {| [a;(b;c)] |} ; [%expect {| a :: (b; c) :: [] |}]

let%expect_test _ = run {| [a] |} ; [%expect {| a :: [] |}]

let%expect_test _ = run {| [] |} ; [%expect {| [] |}]

let%expect_test _ =
  run {| (a :: b) :: c :: d :: [] |} ;
  [%expect {| (a :: b) :: c :: d :: [] |}]

let%expect_test _ =
  run {| (a ; b) ; c ; d ; e |} ;
  [%expect {| (a; b); c; d; e |}]

let%expect_test _ = run {| a, (b, c), d, e |} ; [%expect {| a, (b, c), d, e |}]

let%expect_test _ = run {| a, (b, c) |} ; [%expect {| a, (b, c) |}]

let%expect_test _ = run {| (a, b), c |} ; [%expect {| (a, b), c |}]

let%expect_test _ =
  run {| 1 + - + + 3 |} ; [%expect {| ( + ) 1 (( ~- ) (( ~+ ) (( ~+ ) 3))) |}]

let%expect_test _ = run {| !%< 123; !0 |} ; [%expect {| ( !%< ) 123; ( ! ) 0 |}]

let%expect_test _ = run {| --+1 |} ; [%expect {| ( ~- ) (( ~- ) (( ~+ ) 1)) |}]

let%expect_test _ = run {| f(1+2+3) |} ; [%expect {| f (( + ) (( + ) 1 2) 3) |}]

let%expect_test _ =
  run {| if(a && b) then(1+2) else(3) |} ;
  [%expect {| if ( && ) a b then ( + ) 1 2 else 3 |}]

let%expect_test _ =
  run {| id let a = 1 in a |} ;
  [%expect {| id (let a = 1 in a) |}]

let%expect_test _ =
  run {| ! let a = 1 in a |} ; [%expect {| ( ! ) (let a = 1 in a) |}]

let%expect_test _ =
  run {| 1 + let a = 1 in a |} ;
  [%expect {| ( + ) 1 (let a = 1 in a) |}]

let%expect_test _ = run {| ( a : int ) |} ; [%expect {| (a : int) |}]

let%expect_test _ =
  run {| (fun x -> x : int -> int) |} ;
  [%expect {| (fun x -> x : int -> int) |}]

let%expect_test _ =
  run {| let f x y : int = 1 in f |} ;
  [%expect {| let f = fun x y -> (1 : int) in f |}]

(* ======= Types ======= *)

let%expect_test _ =
  run {| type foo = A of int |} ;
  [%expect {| type foo = A of int |}]

let%expect_test _ =
  run {| type foo = A of int list |} ;
  [%expect {| type foo = A of int list |}]

let%expect_test _ =
  run {| type foo = A of (int, string) map |} ;
  [%expect {| type foo = A of (int, string) map |}]

let%expect_test _ =
  run {| type foo = A of 'a -> 'b -> 'c |} ;
  [%expect {| type foo = A of 'a -> 'b -> 'c |}]

let%expect_test _ =
  run {| type foo = A of 'a * 'b * 'c |} ;
  [%expect {| type foo = A of 'a * 'b * 'c |}]

let%expect_test _ =
  run {| type foo = A of 'some_type_var |} ;
  [%expect {| type foo = A of 'some_type_var |}]

let%expect_test _ =
  run
    {| type foo = A of
         ('a -> int * (string, unit, 'b -> 'c) foo bar option) -> e |} ;
  [%expect
    {|
    type foo =
    | A of
      (
       'a -> int
       * (
          ((string, unit, 'b -> 'c) foo) bar
         ) option
      ) -> e
    |}]

(* ======= Some other stuff ======= *)

let%expect_test _ =
  run {| let (f, s) = (f + s, f - s) |} ;
  [%expect {| let f, s = ( + ) f s, ( - ) f s |}]

let%expect_test _ =
  run {| let (>>=) a b = a ** b |} ;
  [%expect {| let ( >>= ) = fun a b -> ( ** ) a b |}]

let%expect_test _ =
  run {| let (++) a b = a + b |} ;
  [%expect {| let ( ++ ) = fun a b -> ( + ) a b |}]

let%expect_test _ =
  run
    {| let(*sus*)rec(*firstcomment*)f n = (* second comment *) (* third
         comment*) n + 1 |} ;
  [%expect {| let rec f = fun n -> ( + ) n 1 |}]

let%expect_test _ =
  run {| letrec f n = n + 1 |} ;
  [%expect {| ( = ) (letrec f n) (( + ) n 1) |}]

let%expect_test _ = run {| let reca = 1 |} ; [%expect {| let reca = 1 |}]

let%expect_test _ =
  run {| type 'a list = Nil | Cons of 'a * 'a list |} ;
  [%expect {|
    type 'a list =
    | Nil
    | Cons of 'a * 'a list
    |}]

let%expect_test _ = run {| 1a |} ; [%expect {| syntax error |}]

let%expect_test _ = run {| 1 ;; a |} ; [%expect {|
    1;;
    a
    |}]

let%expect_test _ = run {| type foo |} ; [%expect {| type foo |}]

let%expect_test _ =
  run {| type ('a, 'b) bar |} ;
  [%expect {| type ('a, 'b) bar |}]

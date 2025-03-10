(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Lib
open Base
open Format
open Test.Utils

let test code =
  let before = ref [] in
  let after = ref [] in
  infer code (fun ast -> before := ast);
  anf code (fun ast ->
    let anf_prog = Format.asprintf "%a" Anf_ast.pp_program ast in
    infer anf_prog (fun tast -> after := tast));
  Format.printf "Before:@\n%a@\n" Lib.Tast.pp_toplevel_types !before;
  Format.printf "After:@\n%a@\n" Lib.Tast.pp_toplevel_types !after
;;

let%expect_test _ =
  test {|
    let a = 42
  |};
  [%expect {|
    Before:
    a: int

    After:
    a: int
    |}]
;;

let%expect_test _ =
  test {|
    let a = 1 + 2 - 42
  |};
  [%expect {|
    Before:
    a: int

    After:
    a: int
    |}]
;;

let%expect_test _ =
  test
    {|
    let rec fact = fun x -> if x < 2 then 1 else x * fact (x - 1)
    let main = print_int (fact 5)
  |};
  [%expect
    {|
    Before:
    fact: int -> int
    main: ()

    After:
    fact: int -> int
    main: ()
    |}]
;;

let%expect_test _ =
  test
    {|
      let fact n =
        let rec helper n cont =
          if n <= 1 then 
            cont 1
          else 
            helper (n - 1) (fun res -> cont (n * res)) 
        in 
        helper n (fun x -> x)
    |};
  [%expect
    {|
    Before:
    fact: int -> int

    After:
    ll_2: (int -> 'g) -> int -> int -> 'g
    ll_helper_1: int -> (int -> 'x) -> 'x
    ll_3: 'y -> 'y
    fact: int -> int
    |}]
;;

(* let%expect_test _ =
  test
    {|
    let cross (x1, y1, z1) (x2, y2, z2) =
      let x = (y1 * z2) - (z1 * y2) in
      let y = (z1 * x2) - (x1 * z2) in
      let z = (x1 * y2) - (y1 * x2) in
      (x, y, z)
    
    let main = 
      let a = (1, 2, 3) in
      let b = (4, 5, 6) in
      let c = cross a b in
      print_tuple c
    |};
  [%expect
    {|
    Before:
    cross: (int * int * int) -> (int * int * int) -> (int * int * int)
    main: ()

    After:
    cross: 'a -> 'b -> (int * int * int)
    main: ()
    |}]
;; *)

let%expect_test _ =
  test
    {|
    let rec map f list = match list with
      | hd :: tl -> (f hd) :: (map f tl) 
      | [] -> []
  |};
  [%expect
    {|
    Before:
    map: ('e -> 'h) -> 'e list -> 'h list

    After:
    map: ('j -> 'z) -> 'j list -> 'z list
    |}]
;;

let%expect_test _ =
  test
    {| 
  let fib n =
    let rec helper acc n =
      match (n, acc) with
      | (0, x :: _) -> x
      | (_, x :: y :: _) -> helper ((x + y) :: acc) (n - 1)
      | _ -> -1
    in
    helper [ 1; 1 ] (n - 2)
  |};
  [%expect
    {|
    Before:
    fib: int -> int

    After:
    ll_helper_1: int list -> int -> int
    fib: int -> int
    |}]
;;

let%expect_test _ =
  test
    {|
    let add_cps x y = fun k -> k (x + y)
    let square_cps x = fun k -> k (x * x)
    let pythagoras_cps x y = fun k ->
      square_cps x (fun x_squared ->
        square_cps y (fun y_squared ->
          add_cps x_squared y_squared k))
    |};
  [%expect
    {|
    Before:
    add_cps: int -> int -> (int -> 'f) -> 'f
    square_cps: int -> (int -> 'l) -> 'l
    pythagoras_cps: int -> int -> (int -> 'ab) -> 'ab

    After:
    add_cps: int -> int -> (int -> 'g) -> 'g
    square_cps: int -> (int -> 'n) -> 'n
    ll_4: (int -> 'v) -> int -> int -> 'v
    ll_3: (int -> 'ag) -> int -> int -> 'ag
    pythagoras_cps: int -> int -> (int -> 'ar) -> 'ar
    |}]
;;

let%expect_test _ =
  test {|
    let f a0 a1 a2 a3 a4 a5 = a0 + a1 + a2 + a3 + a4 + a5
    |};
  [%expect
    {|
    Before:
    f: int -> int -> int -> int -> int -> int -> int

    After:
    f: int -> int -> int -> int -> int -> int -> int
    |}]
;;

let%expect_test _ =
  test
    {|
    let pow x n =
      let rec helper acc n =
        match n with
        | 0 -> acc
        | n -> helper (acc * x) (n - 1)
      in
      helper 1 n
  |};
  [%expect
    {|
    Before:
    pow: int -> int -> int

    After:
    ll_helper_1: int -> int -> int -> int
    pow: int -> int -> int
    |}]
;;

let%expect_test _ =
  test {|let compose = fun f -> fun g -> fun x -> f (g x)|};
  [%expect
    {|
    Before:
    compose: ('d -> 'e) -> ('c -> 'd) -> 'c -> 'e

    After:
    compose: ('d -> 'f) -> ('c -> 'd) -> 'c -> 'f
    |}]
;;

let%expect_test _ =
  test {|let choose = fun l -> fun r -> fun b -> if b then l else r|};
  [%expect
    {|
    Before:
    choose: 'b -> 'b -> bool -> 'b

    After:
    choose: 'b -> 'b -> bool -> 'b
    |}]
;;

let%expect_test _ =
  test
    {| 
    let rec map = fun f -> fun list -> match list with
    | [] -> []
    | hd::tl -> (f hd) :: (map f tl)

    let rec fold =
      fun init -> fun f -> fun list -> 
      match list with
      | [] -> init
      | hd :: tl -> fold (f init hd) f tl

    let rec filter = fun f -> fun list ->
      match list with
      | [] -> []
      | hd :: tl -> if f hd then hd :: filter f tl else filter f tl 
    
    let gt0 = filter (fun x -> x > 0) 
    let sq = map (fun x -> x * x)
    let sum = fold 0 (fun acc -> fun x -> acc + x)
    let x = [1;2;3]
    let x = sum (sq (gt0 x))
  |};
  [%expect
    {|
    Before:
    map: ('e -> 'f) -> 'e list -> 'f list
    fold: 's -> ('s -> 't -> 's) -> 't list -> 's
    filter: ('af -> bool) -> 'af list -> 'af list
    gt0: int list -> int list
    sq: int list -> int list
    sum: int list -> int
    x: int list
    x: int

    After:
    map: ('n -> 's) -> 'n list -> 's list
    fold: 'bb -> ('bb -> 'ap -> 'bb) -> 'ap list -> 'bb
    filter: ('bp -> bool) -> 'bp list -> 'bp list
    ll_7: int -> bool
    gt0: int list -> int list
    ll_9: int -> int
    sq: int list -> int list
    ll_11: int -> int -> int
    sum: int list -> int
    x_2: int list
    x_3: int
    |}]
;;

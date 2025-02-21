(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Test.Utils

let test = test_inferencer

let%expect_test _ =
  test {| let x = () |};
  [%expect {| x: () |}]
;;

let%expect_test _ =
  test {| let x = true |};
  [%expect {| x: bool |}]
;;

let%expect_test _ =
  test {| let x = 42 |};
  [%expect {| x: int |}]
;;

let%expect_test _ =
  test {| let x = [1; 2; 3] |};
  [%expect {| x: int list |}]
;;

let%expect_test _ =
  test {| let x = [true; 2; 3] |};
  [%expect {| Unification failed on bool and int |}]
;;

let%expect_test _ =
  test {| let x = [[1]; [2]; [3]] |};
  [%expect {| x: int list list |}]
;;

let%expect_test _ =
  test {| let x = (1, true, fun x -> x, (1, 2)) |};
  [%expect {| x: (int * bool * 'a -> 'a * (int * int)) |}]
;;

let%expect_test _ =
  test {| let x = 1 + 2 |};
  [%expect {| x: int |}]
;;

let%expect_test _ =
  test {| let x = 1 + 2 <= 3 |};
  [%expect {| x: bool |}]
;;

let%expect_test _ =
  test {| let id = fun x -> x |};
  [%expect {| id: 'a -> 'a |}]
;;

let%expect_test _ =
  test {| let const = fun x -> 42 |};
  [%expect {| const: 'a -> int |}]
;;

let%expect_test _ =
  test {| let plus_one = fun x -> x + 1 |};
  [%expect {| plus_one: int -> int |}]
;;

let%expect_test _ =
  test {| let muladd = fun x -> fun y -> fun z -> x * y + z |};
  [%expect {| muladd: int -> int -> int -> int |}]
;;

let%expect_test _ =
  test {| let apply = func arg |};
  [%expect {| Undefined variable 'func' |}]
;;

let%expect_test _ =
  test {| let apply = let plus_one = (fun x -> x + 1) in plus_one 2 |};
  [%expect {| apply: int |}]
;;

let%expect_test _ =
  test
    {|
  let compose =
      let plus_one = fun x -> x + 1 in
      let is_neg = fun x -> x < 0 in
      fun x -> is_neg (plus_one x)
  |};
  [%expect {| compose: int -> bool |}]
;;

let%expect_test _ =
  test {| let compose = let func = fun x -> 42 in let func = fun x -> true in func |};
  [%expect {| compose: 'e -> bool |}]
;;

let%expect_test _ =
  test {| let cond = if true then 42 else false |};
  [%expect {| Unification failed on int and bool |}]
;;

let%expect_test _ =
  test {| let cond = if true then 42 else 0 |};
  [%expect {| cond: int |}]
;;

let%expect_test _ =
  test {| let rec fact = fun x -> if x < 2 then 1 else x * fact (x - 1) |};
  [%expect {| fact: int -> int |}]
;;

let%expect_test _ =
  test {| let rec oc = fun x -> x x |};
  [%expect {| Occurs check failed |}]
;;

let%expect_test _ =
  test {|
    let plus_one = fun x -> x + 1
    let is_neg = fun x -> x < 0
  |};
  [%expect {|
    plus_one: int -> int
    is_neg: int -> bool 
  |}]
;;

let%expect_test _ =
  test
    {|
    let add = fun x -> fun y -> x + y
    let add_one = add 1
    let add_two = add 2
    let x = add_one 3
    let y = add_two 3
  |};
  [%expect
    {|
    add: int -> int -> int
    add_one: int -> int
    add_two: int -> int
    x: int
    y: int
  |}]
;;

let%expect_test _ =
  test
    {|      
    let rec helper = fun n -> fun cont ->
      if n < 2 then 
        cont 1
      else 
        helper (n - 1) (fun res -> cont (n * res)) 

    let fact = fun n ->
      helper n (fun x -> x)

    let x = fact 5
  |};
  [%expect {|
    helper: int -> (int -> 'n) -> 'n
    fact: int -> int
    x: int
    |}]
;;

let%expect_test _ =
  test
    {|
      let fact_2 = fun n -> 
         let rec helper = fun n -> fun acc -> fun cont ->
          if n < 2 then
              cont acc
          else 
              helper (n - 1) (n * acc) cont 
         in

         helper n 1 (fun x -> x)
      
      let x = fact_2 5
    |};
  [%expect {|
    fact_2: int -> int
    x: int
  |}]
;;

let%expect_test _ =
  test {| let (x: int) = 42 |};
  [%expect {| x: int |}]
;;

let%expect_test _ =
  test {| let (x: int) = true |};
  [%expect {| Unification failed on int and bool |}]
;;

let%expect_test _ =
  test {| let (id: int->int) = fun x -> x |};
  [%expect {| id: int -> int |}]
;;

let%expect_test _ =
  test {| let (id: int->int) = fun (x: int) -> x |};
  [%expect {| id: int -> int |}]
;;

let%expect_test _ =
  test {| let (id: int->int) = fun (x: bool) -> x |};
  [%expect {| Unification failed on int and bool |}]
;;

let%expect_test _ =
  test {| let id = fun (x: int) -> x |};
  [%expect {| id: int -> int |}]
;;

let%expect_test _ =
  test {| let (const: int) = (fun x -> 42) () |};
  [%expect {| const: int |}]
;;

let%expect_test _ =
  test {|let f = fun (f: int -> int -> int) -> fun x -> fun y -> f x y|};
  [%expect {| f: (int -> int -> int) -> int -> int -> int |}]
;;

let%expect_test _ =
  test {|let compose = fun f -> fun g -> fun x -> f (g x)|};
  [%expect {| compose: ('d -> 'e) -> ('c -> 'd) -> 'c -> 'e  |}]
;;

let%expect_test _ =
  test {|let choose = fun l -> fun r -> fun b -> if b then l else r|};
  [%expect {| choose: 'b -> 'b -> bool -> 'b |}]
;;

let%expect_test _ =
  test {|let choose = fun (l: bool) -> fun r -> fun b -> if b then l else r|};
  [%expect {| choose: bool -> bool -> bool -> bool |}]
;;

let%expect_test _ =
  test
    {|
  let ( + ) = fun a -> fun b -> a || b 
  let a = true + false
  let ( * ) = fun a -> fun b -> fun c -> a * b * c 
  let b = (2 * 3) 4
  |};
  [%expect
    {|
    ( + ): bool -> bool -> bool
    a: bool
    ( * ): int -> int -> int -> int
    b: int |}]
;;

let%expect_test _ =
  test
    {| 
  let x = 1 
  let y = -x
  let z = true
  let w = not z
  let v = not (not z)
  |};
  [%expect {|
    x: int
    y: int
    z: bool
    w: bool
    v: bool |}]
;;

let%expect_test _ =
  test
    {| 
      let rec fib = fun (n: int) -> match n with
      | 0 -> 0
      | 1 -> 1
      | _ -> (fib (n - 1)) + (fib (n - 2))
  |};
  [%expect {| fib: int -> int |}]
;;

let%expect_test _ =
  test {| let rec fix = fun f -> fun x -> f (fix f) x |};
  [%expect {| fix: (('c -> 'f) -> 'c -> 'f) -> 'c -> 'f |}]
;;

let%expect_test _ =
  test
    {| 
    let ((x, y, z):(bool * int * int->bool)) = (not true, 42, fun x -> if x > 0 then true else false)
    let a = x
    let b = y
    let c = z
  |};
  [%expect
    {|
    (x, y, z): (bool * int * int -> bool)
    a: bool
    b: int
    c: int -> bool
    |}]
;;

let%expect_test _ =
  test {| 
    let (x, y, z) = (not true, 42) 
  |};
  [%expect
    {| Mismatched number of arguments in pattern (x, y, z) and expression (bool * int) |}]
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
    map: ('e -> 'f) -> 'e list -> 'f list
    fold: 's -> ('s -> 't -> 's) -> 't list -> 's
    filter: ('af -> bool) -> 'af list -> 'af list
    gt0: int list -> int list
    sq: int list -> int list
    sum: int list -> int
    x: int list
    x: int |}]
;;

let%expect_test _ =
  test "let (x: int list) = []";
  [%expect {| x: int list |}]
;;

let%expect_test _ =
  test "let (x: int list list) = [[1]; []]";
  [%expect {| x: int list list |}]
;;

let%expect_test _ =
  test "let (x: int list list) = [[[]]]";
  [%expect {|  
    Unification failed on int and 'a list|}]
;;

let%expect_test _ =
  test
    {|
    let rec map = fun f -> fun (list: int list) -> match list with
    | [] -> []
    | hd::tl -> (f hd) :: (map f tl) 
    |};
  [%expect {|  
    map: (int -> 'f) -> int list -> 'f list|}]
;;

let%expect_test _ =
  test
    {|
    let rec fix = fun f x -> f (fix f) x
    let fac = fun self n -> if n <= 1 then 1 else n * self (n - 1)
    |};
  [%expect
    {|  
    fix: (('c -> 'f) -> 'c -> 'f) -> 'c -> 'f
    fac: (int -> int) -> int -> int   
    |}]
;;

let%expect_test _ =
  test
    {|
    let rec fix f x = f (fix f) x
    let fac self n = if n <= 1 then 1 else n * self (n - 1)
    |};
  [%expect
    {|  
    fix: (('c -> 'f) -> 'c -> 'f) -> 'c -> 'f
    fac: (int -> int) -> int -> int
    |}]
;;

let%expect_test _ =
  test {|
    let _ = ( 1,2 )
    |};
  [%expect {| _: (int * int) |}]
;;

let%expect_test _ =
  test {| 
    let tup = (not true, 42, fun x -> x)
    let (x, y, z) = tup 
  |};
  [%expect
    {|
    tup: (bool * int * 'a -> 'a)
    (x, y, z): (bool * int * 'c -> 'c)
    |}]
;;

let%expect_test _ =
  test
    {| 
    let sum tuples =
      let rec helper acc tuples =
        match tuples with
        | [] -> acc
        | (a, b, c) :: tl ->
          let (x, y, z) = acc in
          helper (a + x, b + y, c + z) tl
      in
      helper (0, 0, 0) tuples
  |};
  [%expect {| sum: (int * int * int) list -> (int * int * int) |}]
;;

let%expect_test _ =
  test
    {| 
    let f x =
      match x with
      | [ 1 ] -> 1
      | [ a ] -> a
      | hd :: tl -> hd
      | _ -> 42
  |};
  [%expect {| f: int list -> int |}]
;;

let%expect_test _ =
  test {| 
    let f [[a; b]; [c; d]] = a + b + c + d
  |};
  [%expect {| f: int list list -> int |}]
;;

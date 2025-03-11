[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open MTestRun

let%expect_test _ =
  run `SimplOpt {| let 1 = 2 |} ;
  [%expect {| (NotImplemented "patterns") |}]

let code = {| let a = 1 |}

let%expect_test _ = run `SimplOpt code ; [%expect {| (fun a -> ()) 1 |}]

let%expect_test _ =
  run `CLess code ; [%expect {|
    let f0 = fun a -> ();;
    f0 1
    |}]

let code = {| let a = 1 in let b = 2 in b |}

let%expect_test _ = run `SimplOpt code ; [%expect {| (fun a b -> b) 1 2 |}]

let%expect_test _ =
  run `CLess code ; [%expect {|
    let f0 = fun a b -> b;;
    f0 1 2
    |}]

let code =
  {| let a = 1 and b = 2;;
     let f x y = x + y in
     f 5;;
     type foo = Foo of string;;
     a + b |}

let%expect_test _ =
  run `SimplOpt code ;
  [%expect
    {|
    (
     fun b a ->
       (fun f -> f 5) (fun x y -> ( + ) x y);
       ( + ) a b
    ) 2 1
    |}]

let%expect_test _ =
  run `CLess code ;
  [%expect
    {|
    let f0 = fun x y -> ( + ) x y;;
    let f1 = fun f -> f 5;;
    let f2 = fun b a -> f1 f0; ( + ) a b;;
    f2 2 1
    |}]

let%expect_test _ =
  run `Anf code ;
  [%expect
    {|
    let f0 = fun x y -> ( + ) x y;;
    let f1 = fun f -> f 5;;
    let f2 = fun b a -> f1 f0; ( + ) a b;;
    f2 2 1
    |}]

let code = {| let x = 1 + 2 and y = 3 and z = 4 in x - z + y |}

let%expect_test _ =
  run `SimplOpt code ;
  [%expect {| (fun z y x -> ( + ) (( - ) x z) y) 4 (( + ) 1 2) 3 |}]

let%expect_test _ =
  run `CLess code ;
  [%expect
    {|
    let f0 = fun z y x -> ( + ) (( - ) x z) y;;
    f0 4 (( + ) 1 2) 3
    |}]

let%expect_test _ =
  run `Anf code ;
  [%expect
    {|
    let f0 =
      fun z y x -> let v0 = ( - ) x z in ( + ) v0 y;;
    let v0 = ( + ) 1 2 in f0 4 v0 3
    |}]

let code =
  {| let f x y = x + y;;
     f 1 1;;
     let f x y = x - y;;
     f 2 2 |}

let%expect_test _ =
  run `SimplOpt code ;
  [%expect
    {|
    (
     fun f ->
       f 1 1; (fun f -> f 2 2) (fun x y -> ( - ) x y)
    ) (fun x y -> ( + ) x y)
    |}]

let%expect_test _ =
  run `CLess code ;
  [%expect
    {|
    let f0 = fun x y -> ( + ) x y;;
    let f1 = fun x y -> ( - ) x y;;
    let f2 = fun f -> f 2 2;;
    let f3 = fun f -> f 1 1; f2 f1;;
    f3 f0
    |}]

let code = {| (fun x -> fun y -> fun z -> x - 1 + y + z) 1 2 3 |}

let%expect_test _ =
  run `SimplOpt code ;
  [%expect {| (fun x y z -> ( + ) (( + ) (( - ) x 1) y) z) 1 2 3 |}]

let%expect_test _ =
  run `CLess code ;
  [%expect
    {|
    let f0 =
      fun x y z -> ( + ) (( + ) (( - ) x 1) y) z;;
    f0 1 2 3
    |}]

let%expect_test _ =
  run `Anf code ;
  [%expect
    {|
    let f0 =
      fun x y z ->
        let v0 = ( - ) x 1 in
        let v1 = ( + ) v0 y in ( + ) v1 z;;
    f0 1 2 3
    |}]

let code =
  {| let f x y =
     let x z = y + z in
     let y z = x 1 + z in
     x 1 + y 2;; f 5 10 |}

let%expect_test _ =
  run `SimplOpt code ;
  [%expect
    {|
    (fun f -> f 5 10)
      (
       fun x y ->
         (
          fun x ->
            (fun y -> ( + ) (x 1) (y 2))
              (fun z -> ( + ) (x 1) z)
         ) (fun z -> ( + ) y z)
      )
    |}]

let%expect_test _ =
  run `CLess code ;
  [%expect
    {|
    let f0 = fun y z -> ( + ) y z;;
    let f1 = fun x z -> ( + ) (x 1) z;;
    let f2 = fun x y -> ( + ) (x 1) (y 2);;
    let f3 = fun x -> f2 x (f1 x);;
    let f4 = fun x y -> f3 (f0 y);;
    let f5 = fun f -> f 5 10;;
    f5 f4
    |}]

let%expect_test _ =
  run `Anf code ;
  [%expect
    {|
    let f0 = fun y z -> ( + ) y z;;
    let f1 = fun x z -> let v0 = x 1 in ( + ) v0 z;;
    let f2 =
      fun x y ->
        let v0 = y 2 in let v1 = x 1 in ( + ) v1 v0;;
    let f3 = fun x -> let v0 = f1 x in f2 x v0;;
    let f4 = fun x y -> let v0 = f0 y in f3 v0;;
    let f5 = fun f -> f 5 10;;
    f5 f4
    |}]

let code = {| let rec fact n = if n < 2 then 1 else n * fact (n-1) in fact 5 |}

let%expect_test _ =
  run `SimplOpt code ;
  [%expect
    {|
    (fun fact -> fact 5)
      (
       fix
         (
          fun fact n ->
            if ( < ) n 2
            then 1 else ( * ) n (fact (( - ) n 1))
         )
      )
    |}]

let%expect_test _ =
  run `CLess code ;
  [%expect
    {|
    let rec f0 =
      fun n ->
        if ( < ) n 2
        then 1 else ( * ) n (f0 (( - ) n 1));;
    let f1 = fun fact -> fact 5;;
    f1 f0
    |}]

let%expect_test _ =
  run `Anf code ;
  [%expect
    {|
    let rec f0 =
      fun n ->
        let v0 = ( < ) n 2 in
        if v0
        then 1
        else
          let v1 = ( - ) n 1 in
          let v2 = f0 v1 in ( * ) n v2;;
    let f1 = fun fact -> fact 5;;
    f1 f0
    |}]

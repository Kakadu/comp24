[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Stdio

open MTestRun

let run ir path =
  try In_channel.read_all path |> run ir
  with Sys_error _ -> print_endline "failed to read"

let%expect_test _ =
  run `Anf "./manytests/typed/001fac.ml" ;
  [%expect
    {|
    let rec f0 =
      fun n ->
        let v0 = ( <= ) n 1 in
        if v0
        then 1
        else
          let v1 = ( - ) n 1 in
          let v2 = f0 v1 in ( * ) n v2;;
    let f1 = fun main -> ();;
    let f2 =
      fun fac ->
        let v0 = fac 4 in
        let v1 = print_int v0 in f1 0;;
    f2 f0
    |}]

let%expect_test _ =
  run `Anf "./manytests/typed/002fac.ml" ;
  [%expect
    {|
    let f0 = fun k n p -> let v0 = ( * ) p n in k v0;;
    let rec f1 =
      fun n k ->
        let v0 = ( = ) n 1 in
        if v0
        then k 1
        else
          let v2 = f0 k n in
          let v3 = ( - ) n 1 in f1 v3 v2;;
    let f2 = fun print_int -> print_int;;
    let f3 = fun main -> ();;
    let f4 =
      fun fac_cps ->
        let v0 = fac_cps 4 f2 in
        let v1 = print_int v0 in f3 0;;
    f4 f1
    |}]

let%expect_test _ =
  run `Anf "./manytests/typed/003fib.ml" ;
  [%expect
    {|
    let f0 = fun b fib_acc n1 ab -> fib_acc b ab n1;;
    let f1 =
      fun a b fib_acc n1 ->
        let v0 = ( + ) a b in f0 b fib_acc n1 v0;;
    let rec f2 =
      fun a b n ->
        let v0 = ( = ) n 1 in
        if v0
        then b else let v1 = ( - ) n 1 in f1 a b f2 v1;;
    let rec f3 =
      fun n ->
        let v0 = ( < ) n 2 in
        if v0
        then n
        else
          let v1 = ( - ) n 2 in
          let v2 = f3 v1 in
          let v3 = ( - ) n 1 in
          let v4 = f3 v3 in ( + ) v4 v2;;
    let f4 = fun main -> ();;
    let f5 =
      fun fib_acc fib ->
        let v0 = fib_acc 0 1 4 in
        let v1 = print_int v0 in
        let v2 = fib 4 in
        let v3 = print_int v2 in f4 0;;
    let f6 = fun fib_acc -> f5 fib_acc f3;;
    f6 f2
    |}]

let%expect_test _ =
  run `Anf "./manytests/typed/004manyargs.ml" ;
  [%expect
    {|
    let f0 =
      fun f ->
        let v0 = ( = ) 1 1 in if v0 then f else f;;
    let f1 = fun c -> 0;;
    let f2 = fun c b -> let v0 = print_int c in f1 v0;;
    let f3 =
      fun b c a -> let v0 = print_int b in f2 c v0;;
    let f4 =
      fun a b c -> let v0 = print_int a in f3 b c v0;;
    let f5 =
      fun a b c d e f g h i j ->
        let v0 = ( + ) a b in
        let v1 = ( + ) v0 c in
        let v2 = ( + ) v1 d in
        let v3 = ( + ) v2 e in
        let v4 = ( + ) v3 f in
        let v5 = ( + ) v4 g in
        let v6 = ( + ) v5 h in
        let v7 = ( + ) v6 i in ( + ) v7 j;;
    let f6 = fun temp2 -> 0;;
    let f7 =
      fun test3 wrap rez ->
        let v0 = print_int rez in
        let v1 = wrap test3 1 10 100 in f6 v1;;
    let f8 = fun main -> ();;
    let f9 =
      fun test3 wrap test10 ->
        let v0 =
          wrap test10 1 10 100 1000 10000 100000
            1000000 10000000 100000000 1000000000
        in
        let v1 = f7 test3 wrap v0 in f8 v1;;
    let f10 = fun wrap test3 -> f9 test3 wrap f5;;
    let f11 = fun wrap -> f10 wrap f4;;
    f11 f0
    |}]

let%expect_test _ =
  run `Anf "./manytests/typed/005fix.ml" ;
  [%expect
    {|
    let rec f0 = fun f x -> let v0 = f0 f in f v0 x;;
    let f1 =
      fun self n ->
        let v0 = ( <= ) n 1 in
        if v0
        then 1
        else
          let v1 = ( - ) n 1 in
          let v2 = self v1 in ( * ) n v2;;
    let f2 = fun main -> ();;
    let f3 =
      fun fix fac ->
        let v0 = fix fac 6 in
        let v1 = print_int v0 in f2 0;;
    let f4 = fun fix -> f3 fix f1;;
    f4 f0
    |}]

let%expect_test _ =
  run `Anf "./manytests/typed/006partial.ml" ;
  [%expect
    {|
    let f0 = fun foo -> ( * ) foo 10;;
    let f1 = fun foo -> ( + ) foo 2;;
    let f2 = fun b -> if b then f1 else f0;;
    let f3 =
      fun foo x ->
        let v0 = foo (false) x in
        let v1 = foo (true) v0 in
        let v2 = foo (false) v1 in foo (true) v2;;
    let f4 = fun main -> ();;
    let f5 =
      fun foo ->
        let v0 = foo 11 in
        let v1 = print_int v0 in f4 0;;
    let f6 = fun foo -> let v0 = f3 foo in f5 v0;;
    f6 f2
    |}]

let%expect_test _ =
  run `Anf "./manytests/typed/006partial2.ml" ;
  [%expect
    {|
    let f0 =
      fun a b c ->
        let v0 = print_int a in
        let v1 = print_int b in
        let v2 = print_int c in
        let v3 = ( * ) b c in ( + ) a v3;;
    let f1 = fun foo -> let v0 = print_int foo in 0;;
    let f2 = fun foo -> let v0 = foo 3 in f1 v0;;
    let f3 = fun foo -> let v0 = foo 2 in f2 v0;;
    let f4 = fun main -> ();;
    let f5 =
      fun foo ->
        let v0 = foo 1 in let v1 = f3 v0 in f4 v1;;
    f5 f0
    |}]

let%expect_test _ =
  run `Anf "./manytests/typed/006partial3.ml" ;
  [%expect
    {|
    let f0 = fun c -> print_int c;;
    let f1 = fun b -> let v0 = print_int b in f0;;
    let f2 = fun a -> let v0 = print_int a in f1;;
    let f3 = fun main -> ();;
    let f4 = fun foo -> let v0 = foo 4 8 9 in f3 0;;
    f4 f2
    |}]

let%expect_test _ =
  run `Anf "./manytests/typed/007order.ml" ;
  [%expect {| (NotImplemented "patterns") |}]

let%expect_test _ =
  run `Anf "./manytests/typed/008ascription.ml" ;
  [%expect
    {|
    let f0 = fun f g x -> let v0 = g x in f x v0;;
    let f1 =
      fun _start ->
        let v0 = ( / ) _start 2 in ( = ) v0 0;;
    let f2 =
      fun x b -> if b then ( + ) x 1 else ( * ) x 2;;
    let f3 = fun main -> ();;
    let f4 =
      fun addi ->
        let v0 = addi f2 f1 4 in
        let v1 = print_int v0 in f3 0;;
    f4 f0
    |}]

let%expect_test _ =
  run `Anf "./manytests/typed/009let_poly.ml" ;
  [%expect {| (NotImplemented "tuples") |}]

let%expect_test _ =
  run `Anf "./manytests/typed/010sukharev.ml" ;
  [%expect {| (NotImplemented "pattern matching") |}]

let%expect_test _ =
  run `Anf "./manytests/typed/011mapcps.ml" ;
  [%expect {| (NotImplemented "pattern matching") |}]

let%expect_test _ =
  run `Anf "./manytests/typed/012fibcps.ml" ;
  [%expect
    {|
    let f0 = fun a k b -> let v0 = ( + ) a b in k v0;;
    let f1 =
      fun fib k n a ->
        let v0 = f0 a k in
        let v1 = ( - ) n 2 in fib v1 v0;;
    let rec f2 =
      fun n k ->
        let v0 = ( < ) n 2 in
        if v0
        then k n
        else
          let v2 = f1 f2 k n in
          let v3 = ( - ) n 1 in f2 v3 v2;;
    let f3 = fun x -> x;;
    let f4 = fun main -> ();;
    let f5 =
      fun fib ->
        let v0 = fib 6 f3 in
        let v1 = print_int v0 in f4 v1;;
    f5 f2
    |}]

let%expect_test _ =
  run `Anf "./manytests/typed/013foldfoldr.ml" ;
  [%expect {| (NotImplemented "pattern matching") |}]

let%expect_test _ =
  run `Anf "./manytests/typed/015tuples.ml" ;
  [%expect {| (NotImplemented "mutually recursive bindings") |}]

let%expect_test _ =
  run `Anf "./manytests/typed/016lists.ml" ;
  [%expect {| (NotImplemented "pattern matching") |}]

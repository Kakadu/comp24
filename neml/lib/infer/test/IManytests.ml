[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open Stdio

open ITestRun

let run path =
  try In_channel.read_all path |> run
  with Sys_error _ -> print_endline "failed to read"

let%expect_test _ =
  run "./manytests/do_not_type/001.ml" ;
  [%expect {| (UnboundVariable (I "fac")) |}]

let%expect_test _ =
  run "./manytests/do_not_type/002if.ml" ;
  [%expect
    {| (UnificationFail ((Con ((I "int"), [])), (Con ((I "bool"), [])))) |}]

let%expect_test _ =
  run "./manytests/do_not_type/003occurs.ml" ;
  [%expect
    {|
    (OccursIn ((V "gen15"),
       (Arr ((Var (V "gen15")), (Arr ((Var (V "gen13")), (Var (V "gen18"))))))))
    |}]

let%expect_test _ =
  run "./manytests/do_not_type/004let_poly.ml" ;
  [%expect
    {| (UnificationFail ((Con ((I "int"), [])), (Con ((I "bool"), [])))) |}]

let%expect_test _ =
  run "./manytests/do_not_type/015tuples.ml" ;
  [%expect {| (NotVarLHSRec (Tuple ((Var (I "a")), (Var (I "b")), []))) |}]

let%expect_test _ =
  run "./manytests/do_not_type/099.ml" ;
  [%expect {| (NotVarLHSRec (Construct ((I "Some"), (Some (Var (I "x")))))) |}]

let%expect_test _ =
  run "./manytests/typed/001fac.ml" ;
  [%expect {|
    fac: int -> int
    main: int
    |}]

let%expect_test _ =
  run "./manytests/typed/002fac.ml" ;
  [%expect {|
    fac_cps: int -> (int -> 'a) -> 'a
    main: int
    |}]

let%expect_test _ =
  run "./manytests/typed/003fib.ml" ;
  [%expect
    {|
    fib_acc: int -> int -> int -> int
    fib: int -> int
    main: int
    |}]

let%expect_test _ =
  run "./manytests/typed/004manyargs.ml" ;
  [%expect
    {|
    wrap: 'a -> 'a
    test3: int -> int -> int -> int
    test10: int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
    main: int
    |}]

let%expect_test _ =
  run "./manytests/typed/005fix.ml" ;
  [%expect
    {|
    fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
    fac: (int -> int) -> int -> int
    main: int
    |}]

let%expect_test _ =
  run "./manytests/typed/006partial.ml" ;
  [%expect
    {|
    foo: bool -> int -> int
    foo: int -> int
    main: int
    |}]

let%expect_test _ =
  run "./manytests/typed/006partial2.ml" ;
  [%expect {|
    foo: int -> int -> int -> int
    main: int
    |}]

let%expect_test _ =
  run "./manytests/typed/006partial3.ml" ;
  [%expect {|
    foo: int -> int -> int -> unit
    main: int
    |}]

let%expect_test _ =
  run "./manytests/typed/007order.ml" ;
  [%expect
    {|
    (UnificationFail ((Arr ((Con ((I "int"), [])), (Var (V "gen11")))),
       (Con ((I "unit"), []))))
    |}]

let%expect_test _ =
  run "./manytests/typed/008ascription.ml" ;
  [%expect
    {|
    addi: ('a -> bool -> int) -> ('a -> bool) -> 'a -> int
    main: int
    |}]

let%expect_test _ =
  run "./manytests/typed/009let_poly.ml" ;
  [%expect {| temp: int * bool |}]

let%expect_test _ =
  run "./manytests/typed/010sukharev.ml" ;
  [%expect
    {|
    _1: int -> int -> int * 'a -> bool
    _2: int
    _3: (int * string) option
    (UnificationMismatch ([(Var (V "gen2")); (Var (V "gen1"))],
       [(Con ((I "int"), [])); (Con ((I "int"), [])); (Con ((I "int"), []))]))
    |}]

let%expect_test _ =
  run "./manytests/typed/011mapcps.ml" ;
  [%expect
    {|
    map: ('a -> 'b) -> 'a list -> ('b list -> 'c) -> 'c
    iter: ('a -> 'b) -> 'a list -> unit
    main: unit
    |}]

let%expect_test _ =
  run "./manytests/typed/012fibcps.ml" ;
  [%expect {|
    fib: int -> (int -> 'a) -> 'a
    main: unit
    |}]

let%expect_test _ =
  run "./manytests/typed/013foldfoldr.ml" ;
  [%expect
    {|
    id: 'a -> 'a
    fold_right: ('a -> 'b -> 'b) -> 'b -> 'a list -> 'b
    foldl: ('a -> 'b -> 'c) -> 'c -> 'b list -> 'c
    main: unit
    |}]

let%expect_test _ =
  run "./manytests/typed/015tuples.ml" ;
  [%expect
    {|
    fix: (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b
    map: ('a -> 'b) -> 'a * 'a -> 'b * 'b
    fixpoly: ('a -> 'b -> 'c) * ('a -> 'b -> 'c) -> 'a
    feven: 'a * (int -> int) -> int -> int
    fodd: (int -> int) * 'a -> int -> int
    tie: (int -> int) * (int -> int)
    meven: int -> int
    modd: int -> int
    main: int
    |}]

let%expect_test _ =
  run "./manytests/typed/016lists.ml" ;
  [%expect
    {|
    length: 'a list -> int
    length_tail: 'a list -> int
    map: ('a -> 'b) -> 'a list -> 'b list
    append: 'a list -> 'a list -> 'a list
    concat: ('a list) list -> 'a list
    iter: ('a -> unit) -> 'a list -> unit
    cartesian: 'a list -> 'b list -> ('a * 'b) list
    main: int
    |}]

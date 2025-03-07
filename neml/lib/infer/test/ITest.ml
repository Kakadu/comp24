[@@@ocaml.text "/*"]

(** Copyright 2024, Andrei, PavlushaSource *)

(** SPDX-License-Identifier: MIT *)

[@@@ocaml.text "/*"]

open! Base
open ITestRun

let%expect_test _ =
  run {|
    let id1 = fun x -> x in
    id1 42; id1 "hello"
  |} ;
  [%expect {| _: string |}]

let%expect_test _ =
  run {|
    let f id = id 42; id "hello" in
    f (fun x -> x)
  |} ;
  [%expect
    {| (UnificationFail ((Con ((I "int"), [])), (Con ((I "string"), [])))) |}]

let%expect_test _ =
  run {| fun x -> let y = x in y |} ;
  [%expect {| _: 'a -> 'a |}]

let%expect_test _ =
  run {|
    fun x ->
      let y = fun z -> x z in y |} ;
  [%expect {| _: ('a -> 'b) -> 'a -> 'b |}]

let%expect_test _ =
  run {| fun x f -> f x |} ; [%expect {| _: 'a -> ('a -> 'b) -> 'b |}]

let%expect_test _ =
  run {| fun f -> fun x -> f x |} ;
  [%expect {| _: ('a -> 'b) -> 'a -> 'b |}]

let%expect_test _ =
  run {| fun f -> fun x -> g x |} ;
  [%expect {| (UnboundVariable (I "g")) |}]

let%expect_test _ =
  run {|
    fun m -> let y = m in
    let x = y true in x
  |} ;
  [%expect {| _: (bool -> 'a) -> 'a |}]

let%expect_test _ =
  run
    {|
    (fun x -> x + 1)
    ( (fun y -> if y then true else false) false )
  |} ;
  [%expect
    {| (UnificationFail ((Con ((I "int"), [])), (Con ((I "bool"), [])))) |}]

let%expect_test _ =
  run {| fun x -> if x then 42 else x |} ;
  [%expect
    {| (UnificationFail ((Con ((I "int"), [])), (Con ((I "bool"), [])))) |}]

let%expect_test _ =
  run {| fun f -> (fun x -> f (x x)) (fun x -> f (x x)) |} ;
  [%expect
    {| (OccursIn ((V "gen1"), (Arr ((Var (V "gen1")), (Var (V "gen5")))))) |}]

let%expect_test _ =
  run {| fun x y (a, _) -> (x + y - a) = 1 |} ;
  [%expect {| _: int -> int -> int * 'a -> bool |}]

let%expect_test _ =
  run {|
    let x, Some f = 1, Some ( ( + ) 4 )
    in f x |} ;
  [%expect {| _: int |}]

let%expect_test _ =
  run {| Some (1, "hi") |} ; [%expect {| _: (int * string) option |}]

let%expect_test _ = run {| None |} ; [%expect {| _: 'a option |}]

let%expect_test _ = run {| Some |} ; [%expect {| _: 'a -> 'a option |}]

let%expect_test _ =
  run {| None 42 |} ;
  [%expect
    {|
    (UnificationFail ((Arr ((Con ((I "int"), [])), (Var (V "gen1")))),
       (Con ((I "option"), [(Var (V "solve0"))]))))
    |}]

let%expect_test _ =
  run {| None None |} ;
  [%expect
    {|
    (UnificationFail ((Arr ((Var (V "gen3")), (Var (V "gen1")))),
       (Con ((I "option"), [(Var (V "solve0"))]))))
    |}]

let%expect_test _ =
  run {| let Some = Some 1 in 0 |} ;
  [%expect
    {|
    (UnificationFail (
       (Arr ((Var (V "solve0")), (Con ((I "option"), [(Var (V "solve0"))])))),
       (Con ((I "option"), [(Con ((I "int"), []))]))))
    |}]

let%expect_test _ =
  run {| let x, Some x = 1, Some 2 in x |} ;
  [%expect {| (PatVarBoundSeveralTimes (I "x")) |}]

let%expect_test _ =
  run {| fun x x -> x |} ; [%expect {| (PatVarBoundSeveralTimes (I "x")) |}]

let%expect_test _ =
  run {| let a, _ = 1, 2, 3 in a |} ;
  [%expect
    {|
    (UnificationMismatch ([(Var (V "gen1")); (Var (V "gen0"))],
       [(Con ((I "int"), [])); (Con ((I "int"), [])); (Con ((I "int"), []))]))
    |}]

let%expect_test _ =
  run {| let a = 1, (fun (a, _) -> a), 2 in a|} ;
  [%expect {| _: int * ('a * 'b -> 'a) * int |}]

let%expect_test _ =
  run
    {|
    match Some id with
      | Some x -> x "hi"; x 5
      | None -> 1
    |} ;
  [%expect {| _: int |}]

let%expect_test _ =
  run
    {|
    fun x ->
      match x with
        | Some v -> Some (v + 1)
        | None -> None
    |} ;
  [%expect {| _: int option -> int option |}]

let%expect_test _ =
  run {| function Some x -> x | None -> 0 |} ;
  [%expect {| _: int option -> int |}]

let%expect_test _ =
  run {| function Some id -> id "hi"; id 5 | None -> 1 |} ;
  [%expect
    {| (UnificationFail ((Con ((I "string"), [])), (Con ((I "int"), [])))) |}]

let%expect_test _ =
  run {| fun arg -> match arg with Some x -> let y = x in y |} ;
  [%expect {| _: 'a option -> 'a |}]

let%expect_test _ =
  run {| function [x] -> let y = x in y |} ;
  [%expect {| _: 'a list -> 'a |}]

let%expect_test _ =
  run {| function 42 -> true | _ -> false |} ;
  [%expect {| _: int -> bool |}]

let%expect_test _ =
  run {| let rec fact n = if n < 2 then 1 else n * fact (n - 1) in fact |} ;
  [%expect {| _: int -> int |}]

let%expect_test _ =
  run {| let rec fact n = if n < 2 then 1 else n * fact true in fact |} ;
  [%expect
    {| (UnificationFail ((Con ((I "bool"), [])), (Con ((I "int"), [])))) |}]

let%expect_test _ =
  run {| let rec fact n = if n < 2 then 1 else n * fact (n - 1)  |} ;
  [%expect {| fact: int -> int |}]

let%expect_test _ =
  run {| let rec fact n = if n < 2 then 1 else n * fact true  |} ;
  [%expect
    {| (UnificationFail ((Con ((I "bool"), [])), (Con ((I "int"), [])))) |}]

let%expect_test _ =
  run {| let rec f x = f 5 in f |} ;
  [%expect {| _: int -> 'a |}]

let%expect_test _ =
  run {| let rec _ = id in 1 |} ;
  [%expect {| (NotVarLHSRec Any) |}]

let%expect_test _ =
  run {| let rec _ = id |} ; [%expect {| (NotVarLHSRec Any) |}]

let%expect_test _ =
  run {| let rec Some x = Some 1 in x |} ;
  [%expect {| (NotVarLHSRec (Construct ((I "Some"), (Some (Var (I "x")))))) |}]

let%expect_test _ = run {| let f x = x |} ; [%expect {| f: 'a -> 'a |}]

let%expect_test _ =
  run {| let id1, id2 = id, id |} ;
  [%expect {|
    id1: 'a -> 'a
    id2: 'a -> 'a
    |}]

let%expect_test _ =
  run {| let Some a = (<) |} ;
  [%expect
    {|
    (UnificationFail ((Con ((I "option"), [(Var (V "solve0"))])),
       (Arr ((Con ((I "int"), [])),
          (Arr ((Con ((I "int"), [])), (Con ((I "bool"), []))))))
       ))
    |}]

let%expect_test _ =
  run {| let Some x = Some id |} ;
  [%expect {| x: 'a -> 'a |}]

let%expect_test _ =
  run {| let () = id |} ;
  [%expect
    {|
    (UnificationFail ((Con ((I "unit"), [])),
       (Arr ((Var (V "solve0")), (Var (V "solve0"))))))
    |}]

let%expect_test _ =
  run {| let [a; b] = [(1,2); (3,4)] |} ;
  [%expect {|
    a: int * int
    b: int * int
    |}]

let%expect_test _ =
  run {| let [Some(a, b)] = [Some(1,2)] |} ;
  [%expect {|
    a: int
    b: int
    |}]

let%expect_test _ =
  run {| let rec x = x + 1 |} ;
  [%expect
    {|
    (NotAllowedRHSRec
       (Apply ((Apply ((Id (I "+")), (Id (I "x")))), (Const (Int 1)))))
    |}]

let%expect_test _ =
  run {| let rec x = x + 1 in x |} ;
  [%expect
    {|
    (NotAllowedRHSRec
       (Apply ((Apply ((Id (I "+")), (Id (I "x")))), (Const (Int 1)))))
    |}]

let%expect_test _ =
  run {| let rec y = 1 in let rec x = y in x |} ;
  [%expect {| _: int |}]

let%expect_test _ =
  run
    {|
      type 'a list = Nil | Cons of 'a * 'a list
      type unit = ()
      type foo = Foo
    |} ;
  [%expect
    {|
    Cons: 'a * 'a list -> 'a list
    Nil: 'a list
    (): unit
    Foo: foo
    |}]

let%expect_test _ =
  run {| type foo = Foo of 'a list |} ;
  [%expect {| (UnboundTypeVariable (V "a")) |}]

let%expect_test _ =
  run {| type foo = Foo of list |} ;
  [%expect {| (TypeArityMismatch (I "list")) |}]

let%expect_test _ =
  run {| type foo = Foo of bar |} ;
  [%expect {| (UnboundType (I "bar")) |}]

let%expect_test _ =
  run {|
    type foo = Foo;;
    type 'a foo = Foo of foo |} ;
  [%expect {|
    Foo: foo
    (TypeArityMismatch (I "foo"))
    |}]

let%expect_test _ =
  run {| function Some x | Some y -> 0 |} ;
  [%expect {| (NotImplemented "or patterns") |}]

let%expect_test _ =
  run {| function Some x | Some (Some x) -> 1 |} ;
  [%expect {| (NotImplemented "or patterns") |}]

let%expect_test _ =
  run {| function Some 1 | Some "42" -> 0 |} ;
  [%expect {| (NotImplemented "or patterns") |}]

let%expect_test _ =
  run {| function 1 | 2 -> 0 |} ;
  [%expect {| (NotImplemented "or patterns") |}]

let%expect_test _ =
  run {| let f (x: 'a) (y: 'b) = (x + 5: 'b) |} ;
  [%expect {| f: int -> int -> int |}]

let%expect_test _ =
  run {| let rec f _ = g 47 and g _ = f 42 |} ;
  [%expect {|
    f: int -> 'a
    g: int -> 'a
    |}]

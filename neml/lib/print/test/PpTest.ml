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
      PPrint.ToChannel.pretty 1. 50 stdout (LPrint.pp_structure str)

let%expect_test _ =
  run
    {|
    let 33 | 1 | 2 | 3 | 1 | 2 | 3 | (1 | 2 | 3) | 1
      | 2 | 3 | 1 | 2 | 3 | 1 | 2 | 3 | 1 | 2
      | (
         3 | 2 | 3 | 1 | 2 | 3 | 1 | 2 | 3 | 1 | 2 | 3
         | 1 | 2 | 3 | 1
        ) | 2 | 1 | 2 | 3
      | (
         22 | 3 | 2 | 3 | 2 | 3 | 2 | 3 | 2 | 3 | 2
         | 3 | 2 | 3 | 2 | 3 | 2 | 3 | 2 | 3 | 2 | 3
         | 2 | 3 | 2 | 3 | 3 | 4
        ) = ()
    |} ;
  [%expect
    {|
    let 33 | 1 | 2 | 3 | 1 | 2 | 3 | (1 | 2 | 3) | 1
      | 2 | 3 | 1 | 2 | 3 | 1 | 2 | 3 | 1 | 2
      | (
         3 | 2 | 3 | 1 | 2 | 3 | 1 | 2 | 3 | 1 | 2 | 3
         | 1 | 2 | 3 | 1
        ) | 2 | 1 | 2 | 3
      | (
         22 | 3 | 2 | 3 | 2 | 3 | 2 | 3 | 2 | 3 | 2
         | 3 | 2 | 3 | 2 | 3 | 2 | 3 | 2 | 3 | 2 | 3
         | 2 | 3 | 2 | 3 | 3 | 4
        ) = ()
    |}]

let%expect_test _ =
  run
    {|
    let 1, 2, 3, 4, 5, 6,
      (
       7, 8, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98,
       98, 98, 98, 98, 98, 98, 98, 98, 98, 9
      ), 10, (11 | 2), 12, 13, 14, 15, 16 = ()
    |} ;
  [%expect
    {|
    let 1, 2, 3, 4, 5, 6,
      (
       7, 8, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98,
       98, 98, 98, 98, 98, 98, 98, 98, 98, 9
      ), 10, (11 | 2), 12, 13, 14, 15, 16 = ()
    |}]

let%expect_test _ =
  run
    {|
    let a :: b,
      f :: (
            c :: dd :: e,
            ed :: eed :: eed :: eed :: eed :: eed :: eed
            :: eed :: eed :: eed :: eed :: eed :: eed :: eed
            :: eed :: eed :: eed :: eed :: eed :: ee :: ee
            :: f :: f
           ) :: g :: h :: jh :: jh :: jh :: jh :: jh :: jh
      :: jh :: jh :: jh :: jh :: j = ()
    |} ;
  [%expect
    {|
    let a :: b,
      f :: (
            c :: dd :: e,
            ed :: eed :: eed :: eed :: eed :: eed :: eed
            :: eed :: eed :: eed :: eed :: eed :: eed :: eed
            :: eed :: eed :: eed :: eed :: eed :: ee :: ee
            :: f :: f
           ) :: g :: h :: jh :: jh :: jh :: jh :: jh :: jh
      :: jh :: jh :: jh :: jh :: j = ()
    |}]

let%expect_test _ =
  run
    {|
    let Some Some Some Some Some Some Some Some Some Some
      Some Some Some Some Some Some Some Some Some Some
      Some Some Some Some Some Some Some Some Some Some
      Some Some Some (
                      1, 2, 3, 3, 4, 5, 5, 5, 5, 5, 5,
                      5, 5, 5, 5, 5, 5, 5, 5
                     ) = ()
    |} ;
  [%expect
    {|
    let Some Some Some Some Some Some Some Some Some Some
      Some Some Some Some Some Some Some Some Some Some
      Some Some Some Some Some Some Some Some Some Some
      Some Some Some (
                      1, 2, 3, 3, 4, 5, 5, 5, 5, 5, 5,
                      5, 5, 5, 5, 5, 5, 5, 5
                     ) = ()
    |}]

let%expect_test _ =
  run
    {|
    (
     a, b, (c, d) e, f, e, ef, e, e, f, e, e, f, e, e,
     f, e, e, f, e, e, f, e, e, f, e, e, f, e, e, f, e,
     e, f, e, e, f, e, e, f, e, e, f, e, e, f, e, e
    ) e
    |} ;
  [%expect
    {|
    (
     a, b, (c, d) e, f, e, ef, e, e, f, e, e, f, e, e,
     f, e, e, f, e, e, f, e, e, f, e, e, f, e, e, f, e,
     e, f, e, e, f, e, e, f, e, e, f, e, e, f, e, e
    ) e
    |}]

let%expect_test _ =
  run
    {|
    (
     a
     : a -> b -> cb -> cb -> cb -> cb -> cb -> cb -> cb
     -> ccb -> cbcb -> cbcb -> cbcb -> (
                                        cbcb -> cbcb -> cbcb
                                        -> cbcb -> cbcb
                                        -> cbcb -> cbcb
                                        -> cbcb -> cbcb
                                        -> cbcb -> cbcb
                                        -> cbcb -> cbcb
                                       ) -> cbcb -> cbcb
     -> cb
    )
    |} ;
  [%expect
    {|
    (
     a
     : a -> b -> cb -> cb -> cb -> cb -> cb -> cb -> cb
     -> ccb -> cbcb -> cbcb -> cbcb -> (
                                        cbcb -> cbcb -> cbcb
                                        -> cbcb -> cbcb
                                        -> cbcb -> cbcb
                                        -> cbcb -> cbcb
                                        -> cbcb -> cbcb
                                        -> cbcb -> cbcb
                                       ) -> cbcb -> cbcb
     -> cb
    )
    |}]

let%expect_test _ =
  run
    {|
    (
     a
     : (a -> b) * cc * cc * cc * cc * cc * cc * cc
     * cc * cc * cc * cc * cc
     * (cc * cc * cc * cc * cc * cc) * cc * cc * cc
     * cc * cc * cc * cc * cc * cc * cc * cc * cc * cc
     * cc * cc * c * c
    )
    |} ;
  [%expect
    {|
    (
     a
     : (a -> b) * cc * cc * cc * cc * cc * cc * cc
     * cc * cc * cc * cc * cc
     * (cc * cc * cc * cc * cc * cc) * cc * cc * cc
     * cc * cc * cc * cc * cc * cc * cc * cc * cc * cc
     * cc * cc * c * c
    )
    |}]

let%expect_test _ =
  run
    {|
    let a, b, c, d, e, f, c, a, a, a, a, a, a, a, a, a,
      a, a, a, a, a, a, a, a, a, a, a = 1
    and
      a, b, c, d, e, f, c, a, a, a, a, a, a, a, a, a,
      a, a, a, a, a, a, a, a, a, a, a =
      a, b, c, d, e, f, c, a, a, a, a, a, a, a, a, a,
      a, a, a, a, a, a, a, a, a, a, a
    and
      b = 2
    and
      c = 3
    in
    a, b, c, d, e, f, c, a, a, a,
    (
     let b = a in
     1, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a
    )
    |} ;
  [%expect
    {|
    let a, b, c, d, e, f, c, a, a, a, a, a, a, a, a, a,
      a, a, a, a, a, a, a, a, a, a, a = 1
    and
      a, b, c, d, e, f, c, a, a, a, a, a, a, a, a, a,
      a, a, a, a, a, a, a, a, a, a, a =
      a, b, c, d, e, f, c, a, a, a, a, a, a, a, a, a,
      a, a, a, a, a, a, a, a, a, a, a
    and
      b = 2
    and
      c = 3
    in
    a, b, c, d, e, f, c, a, a, a,
    (
     let b = a in
     1, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a
    )
    |}]

let%expect_test _ =
  run {|let a = 1 and b=2 in b|} ;
  [%expect {| let a = 1 and b = 2 in b |}]

let%expect_test _ =
  run
    {|
    1, 2, 3, 4, 5,
    let a =
      let a = 1 and b = 2 in
      b, b, b, b, b, b, b, (let a = 2 in b, c), c, c,
      c, c, c, c, c, c, c, c, c, c
    in
    b
    |} ;
  [%expect
    {|
    1, 2, 3, 4, 5,
    (
     let a =
       let a = 1 and b = 2 in
       b, b, b, b, b, b, b, (let a = 2 in b, c), c, c,
       c, c, c, c, c, c, c, c, c, c
     in
     b
    )
    |}]

let%expect_test _ =
  run
    {|
    fun a b c d b c d b c d b c d b c d b c d b c d b
    c d b c d b c d b c d b c d ->
      aaaa, aaaa, aaaa, aaaa, aaaa, aaaa, aaaa, aaaa,
      aaaa, aaaa, aaaa, aaaa, aaaa, aaaa
    |} ;
  [%expect
    {|
    fun a b c d b c d b c d b c d b c d b c d b c d b
    c d b c d b c d b c d b c d ->
      aaaa, aaaa, aaaa, aaaa, aaaa, aaaa, aaaa, aaaa,
      aaaa, aaaa, aaaa, aaaa, aaaa, aaaa
    |}]

let%expect_test _ =
  run
    {|
    f x y d x x x x x x
      ( xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, x, x )
      x x x x x x x x x x x x x x x x x x
    |} ;
  [%expect
    {|
    f x y d x x x x x x
      (
       xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx, xx,
       xx, xx, xx, xx, xx, x, x
      ) x x x x x x x x x x x x x x x x x x
    |}]

let%expect_test _ = run {|function a -> b|} ; [%expect {| function a -> b |}]

let%expect_test _ =
  run {|function a -> b | c -> d|} ;
  [%expect {| function a -> b | c -> d |}]

let%expect_test _ =
  run
    {|
    function
    | a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
    d, d, d ->
      a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
      d, d, d
    | a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
    d, d, d ->
      a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
      d, d, d
    | a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
    d, d, d ->
      a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
      d, d, d
    |} ;
  [%expect
    {|
    function
    | a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
    d, d, d ->
      a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
      d, d, d
    | a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
    d, d, d ->
      a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
      d, d, d
    | a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
    d, d, d ->
      a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
      d, d, d
    |}]

let%expect_test _ =
  run
    {|
    match
      a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a,
      a, a, a, a, a, a, a, a, a, a, a
    with
    | a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
    d, d, d ->
      a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
      d, d, d
    | a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
    d, d, d ->
      a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
      d, d, d
    | a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
    d, d, d ->
      a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
      d, d, d
    |} ;
  [%expect
    {|
    match
      a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a,
      a, a, a, a, a, a, a, a, a, a, a
    with
    | a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
    d, d, d ->
      a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
      d, d, d
    | a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
    d, d, d ->
      a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
      d, d, d
    | a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
    d, d, d ->
      a, d, d, d, d, d, d, d, d, d, d, d, d, d, d, d,
      d, d, d
    |}]

let%expect_test _ =
  run {| match a with a -> b | c -> d|} ;
  [%expect {| match a with a -> b | c -> d |}]

let%expect_test _ =
  run
    {| match aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa with b -> c | c-> d|} ;
  [%expect
    {|
    match
      aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    with b -> c | c -> d
    |}]

let%expect_test _ =
  run
    {| match aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa with b -> c | c-> d|} ;
  [%expect
    {|
    match
      aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    with b -> c | c -> d
    |}]

let%expect_test _ =
  run
    {|
    if aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    then aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    else
      aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    |} ;
  [%expect
    {|
    if aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    then aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    else
      aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    |}]

let%expect_test _ =
  run {|if a then b else c|} ; [%expect {| if a then b else c |}]

let%expect_test _ =
  run {|if aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa then b|} ;
  [%expect
    {|
    if aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    then b
    |}]

let%expect_test _ =
  run
    {|
    if aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    then
      bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
    else c
    |} ;
  [%expect
    {|
    if aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    then
      bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
    else c
    |}]

let%expect_test _ =
  run
    {|a;b;c;d;e;f;g;hf;g;hf;g;hf;g;hf;g;hf;g;hf;g;hf;g;hf;g;hf;g;hf;g;hf;g;hf;g;h|} ;
  [%expect
    {|
    a; b; c; d; e; f; g; hf; g; hf; g; hf; g; hf; g;
    hf; g; hf; g; hf; g; hf; g; hf; g; hf; g; hf; g;
    hf; g; h
    |}]

let%expect_test _ =
  run
    {|let x = A (1,2,3,4,5,6,3,4,5,6,3,4,5,6,3,4,5,6,3,4,5,6,3,4,5,6,3,4,5,6,3,4,5,6,3,4,5,6,7) in x|} ;
  [%expect
    {|
    let x =
      A
      (
       1, 2, 3, 4, 5, 6, 3, 4, 5, 6, 3, 4, 5, 6, 3, 4,
       5, 6, 3, 4, 5, 6, 3, 4, 5, 6, 3, 4, 5, 6, 3, 4,
       5, 6, 3, 4, 5, 6, 7
      )
    in
    x
    |}]

let%expect_test _ =
  run
    {|
    ( avvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
     : aa -> ba -> ba -> ba -> ba -> ba -> ba -> ba -> ba
     -> ba -> ba -> ba -> ba -> ba -> ba -> ba -> ba -> ba
     -> ba -> ba -> ba -> b -> b )
    |} ;

  [%expect
    {|
    (
     avvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
     : aa -> ba -> ba -> ba -> ba -> ba -> ba -> ba -> ba
     -> ba -> ba -> ba -> ba -> ba -> ba -> ba -> ba -> ba
     -> ba -> ba -> ba -> b -> b
    )
    |}]

let%expect_test _ =
  run
    {|type foo = A of string -> string -> string -> string -> string -> string | B of int |} ;
  [%expect
    {|
    type foo =
    | A of
      string -> string -> string -> string -> string -> string
    | B of int
    |}]

let%expect_test _ =
  run
    {|
    type (
          'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a,
          'a, 'a, 'a, 'a, 'a, 'a, 'a, 'a
         ) foo =
    | A of
      string -> string -> string -> string -> string -> string
    | B of int
    |} ;
  [%expect
    {|
    type 'a foo =
    | A of
      string -> string -> string -> string -> string -> string
    | B of int
    |}]

let%expect_test _ =
  run
    {|type ('a, 'a,'a,'a,'a,'a,'a,'a,'a,'a,'a,'a,'a,'a,'a,'a,'a,'a) foo = A of string |} ;
  [%expect {| type 'a foo = A of string |}]

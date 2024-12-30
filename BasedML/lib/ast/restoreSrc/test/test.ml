(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Restore_src.RestoreSrc

let test_restore_src string_src =
  let res = Parser.parse_program string_src in
  match res with
  | Result.Ok prog -> Format.printf "%s" (restore_declarations prog)
  | Result.Error e -> Format.printf "Parser error: %s" e
;;

let%expect_test "Consts" =
  test_restore_src
    {|
  let a = 1;;
  let b = true;;
  let c = [];;
  let g = ();;|};
  [%expect
    {|
    let  a = 1;;
    let  b = true;;
    let  c = [];;
    let  g = ();; |}]
;;

let%expect_test "Patterns" =
  test_restore_src
    {| let r = match x with 
  | _ -> 0
  | head :: lst -> 1
  | a -> 2
  | (f, s) -> 3
  | 2 -> 4
  | (x: int) -> 5;;|};
  [%expect
    {|
    let  r = (match x with
    | _ -> 0
    | (head :: lst) -> 1
    | a -> 2
    | (f, s) -> 3
    | 2 -> 4
    | (x : int) -> 5);; |}]
;;

let%expect_test "Expressions" =
  test_restore_src
    {|
  let id = constant;;
  let funct = fun x -> x;;
  let app = funct id;;
  let ifthen = if (id = 2) then true else false;;
  let letinval = let a = 4 in a * 4;;
  let tup = (1, []);;
  let m = match constant with
  | 1 -> 1
  | 2 -> 3
  | _-> 2;;
  let constriant = (true: int);;
  let constant = 2;;|};
  [%expect
    {|
    let  id = constant;;
    let  funct = (fun x -> x);;
    let  app = (funct id);;
    let  ifthen = (if (((( = ) id) 2)) then true else (false));;
    let  letinval = (let  a = 4 in ((( * ) a) 4));;
    let  tup = (1, []);;
    let  m = (match constant with
    | 1 -> 1
    | 2 -> 3
    | _ -> 2);;
    let  constriant = (true : int);;
    let  constant = 2;; |}]
;;

let%expect_test "Declarations" =
  test_restore_src
    {|
  let a = 2;;
  let rec b = b + a;;
  let c = 2 and d = 0;;
  let rec e = 2 * h and h = 3 * e;;|};
  [%expect
    {|
    let  a = 2;;
    let rec b = ((( + ) b) a);;
    let  c = 2 and d = 0;;
    let rec e = ((( * ) 2) h) and h = ((( * ) 3) e);; |}]
;;

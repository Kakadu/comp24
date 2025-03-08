(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open LTypes
open Infer
module Format = Stdlib.Format
open Stdio

let run code =
  match LParse.parse code with
  | Some [str_item] -> (
    match infer_structure_item StdEnv.env str_item with
    | Ok (env, bounds, sc) ->
        Option.iter sc ~f:(Format.printf "%a\n" Scheme.pp) ;
        List.iter bounds ~f:(fun bound_var ->
            let sc = Env.find_exn env bound_var in
            let (Ident name) = bound_var in
            Format.printf "%s: %a\n" name Scheme.pp sc )
    | Error err ->
        TyError.pp Format.std_formatter err )
  | None ->
      print_endline "syntax error"
  | _ ->
      print_endline "invalid test"

let%expect_test _ =
  run {| fun x -> let y = x in y |} ;
  [%expect {|
    'a. 'a -> 'a |}]

let%expect_test _ =
  run {|
    fun x ->
      let y = fun z -> x z in y |} ;
  [%expect {|
    'a 'b. ('a -> 'b) -> 'a -> 'b |}]

let%expect_test _ =
  run {| fun x f -> f x |} ; [%expect {|
    'a 'b. 'a -> ('a -> 'b) -> 'b |}]

let%expect_test _ =
  run {| fun f -> fun x -> f x |} ;
  [%expect {|
    'a 'b. ('a -> 'b) -> 'a -> 'b |}]

let%expect_test _ =
  run {| fun f -> fun x -> g x |} ;
  [%expect {| (UnboundVariable (Ident "g")) |}]

let%expect_test _ =
  run {|
    fun m -> let y = m in
    let x = y true in x
  |} ;
  [%expect {|
    'a. (bool -> 'a) -> 'a |}]

let%expect_test _ =
  run
    {|
    (fun x -> x + 1)
    ( (fun y -> if y then true else false) false )
  |} ;
  [%expect {|
    (UnificationFail (int, bool)) |}]

let%expect_test _ =
  run {| fun x -> if x then 42 else x |} ;
  [%expect {|
    (UnificationFail (int, bool)) |}]

let%expect_test _ =
  run {| fun f -> (fun x -> f (x x)) (fun x -> f (x x)) |} ;
  [%expect {|
    (OccursIn ('gen1, 'gen1 -> 'gen5)) |}]

let%expect_test _ =
  run {| fun x y (a, _) -> (x + y - a) = 1 |} ;
  [%expect {|
    'a. int -> int -> (int * 'a) -> bool |}]

let%expect_test _ =
  run {|
    let x, Some f = 1, Some ( ( + ) 4 )
    in f x |} ;
  [%expect {|
    int |}]

let%expect_test _ = run {| Some (1, "hi") |} ; [%expect {|
    syntax error |}]

let%expect_test _ = run {| None |} ; [%expect {|
    'a. 'a option |}]

let%expect_test _ =
  run {| Some |} ; [%expect {|
    (ConstructorArityMismatch (Ident "Some")) |}]

let%expect_test _ =
  run {| None 42 |} ; [%expect {| (ConstructorArityMismatch (Ident "None")) |}]

let%expect_test _ =
  run {| None None |} ;
  [%expect {| (ConstructorArityMismatch (Ident "None")) |}]

let%expect_test _ =
  run {| let Some = Some 1 in 0 |} ;
  [%expect {| (ConstructorArityMismatch (Ident "Some")) |}]

let%expect_test _ =
  run {| let x, Some x = 1, Some 2 in x |} ;
  [%expect {| (PatVarBoundSeveralTimes (Ident "x")) |}]

let%expect_test _ =
  run {| fun x x -> x |} ; [%expect {| (PatVarBoundSeveralTimes (Ident "x")) |}]

let%expect_test _ =
  run {| let a, _ = 1, 2, 3 in a |} ;
  [%expect {| UnificationMismatch |}]

let%expect_test _ =
  run {|
    match Some id with
      | Some x -> x 2
      | None -> 1
    |} ;
  [%expect {| int |}]

let%expect_test _ =
  run
    {|
    fun x ->
      match x with
        | Some v -> Some (v + 1)
        | None -> None
    |} ;
  [%expect {| int option -> int option |}]

let%expect_test _ =
  run {| function Some x -> x | None -> 0 |} ;
  [%expect {| syntax error |}]

let%expect_test _ =
  run {| function Some id -> id "hi"; id 5 | None -> 1 |} ;
  [%expect {| syntax error |}]

let%expect_test _ =
  run {| fun arg -> match arg with Some x -> let y = x in y |} ;
  [%expect {| 'a. 'a option -> 'a |}]

let%expect_test _ =
  run {| function [x] -> let y = x in y |} ;
  [%expect {| 'a 'b. 'a -> 'b list -> 'b |}]

let%expect_test _ =
  run {| function 42 -> true | _ -> false |} ;
  [%expect {| syntax error |}]

let%expect_test _ =
  run {| let rec fact n = if n < 2 then 1 else n * fact (n - 1) in fact |} ;
  [%expect {| int -> int |}]

let%expect_test _ =
  run {| let rec fact n = if n < 2 then 1 else n * fact true in fact |} ;
  [%expect {| (UnificationFail (int, bool)) |}]

let%expect_test _ =
  run {| let rec f x = f 5 in f |} ;
  [%expect {| 'a. int -> 'a |}]

let%expect_test _ =
  run {| let rec _ = id in 1 |} ;
  [%expect {| NotVarLHSRec |}]

let%expect_test _ =
  run {| let rec Some x = Some 1 in x |} ;
  [%expect {| NotVarLHSRec |}]

let%expect_test _ =
  run {| let rec fact n = if n < 2 then 1 else n * fact (n - 1)  |} ;
  [%expect {|
    fact: int -> int |}]

let%expect_test _ =
  run {| let rec fact n = if n < 2 then 1 else n * fact true  |} ;
  [%expect {| (UnificationFail (bool, int)) |}]

let%expect_test _ =
  run {| let rec x = x + 1|} ;
  [%expect
    {|
    (NotAllowedRHSRec
       (Exp_apply (
          (Exp_apply ((Exp_ident (Ident "+")), (Exp_ident (Ident "x")))),
          (Exp_constant (Const_integer 1))))) |}]

let%expect_test _ =
  run {| let rec x = x + 1 in x |} ;
  [%expect
    {|
    (NotAllowedRHSRec
       (Exp_apply (
          (Exp_apply ((Exp_ident (Ident "+")), (Exp_ident (Ident "x")))),
          (Exp_constant (Const_integer 1))))) |}]

let%expect_test _ =
  run {| let rec y = 1 in let rec x = y in x |} ;
  [%expect {|
    int |}]

let%expect_test _ =
  run
    {|
    let map f l =
      let rec helper acc = function
        | [] -> acc []
        | x :: xs -> helper (fun ys -> acc (f x :: ys)) xs
      in
      helper (fun ys -> ys) l
    |} ;
  [%expect {|
    syntax error |}]

let%expect_test _ =
  run
    {|
    let rev l =
      let rec helper acc = function
        | [] -> acc
        | hd :: tl -> helper (hd :: acc) tl
      in
      helper [] l
    |} ;
  [%expect {|
    syntax error |}]

let%expect_test _ =
  run
    {|
    let rec list_iter f = function
      | [] -> ()
      | hd :: tl -> let () = f hd in list_iter f tl
    |} ;
  [%expect {|
    syntax error |}]

let%expect_test _ =
  run {| function Some "42" | Some 42 -> 0 |} ;
  [%expect {| syntax error |}]

let%expect_test _ =
  run {| function Some x | Some y -> 0 |} ;
  [%expect {| syntax error |}]

let%expect_test _ =
  run {| let f (x : int) = x + 1 |} ;
  [%expect {|
    f: int -> int |}]

let%expect_test _ =
  run {| let f (x : string) = x + 1 |} ;
  [%expect {|
    (UnificationFail (string, int)) |}]

let%expect_test _ =
  run {| (fun x -> x + 1 : int -> string) |} ;
  [%expect {| (UnificationFail (string, int)) |}]

let%expect_test _ =
  run {| (fun x -> x + 1 : int -> int) |} ;
  [%expect {| int -> int |}]

let%expect_test _ =
  run {| let f x : int = x + 1 in f |} ;
  [%expect {| int -> int |}]

let%expect_test _ =
  run
    {|
    let map f l =
      let rec helper acc = fun x -> match x with
        | [] -> acc []
        | x :: xs -> helper (fun ys -> acc (f x :: ys)) xs
      in
      helper (fun ys -> ys) l
    |} ;
  [%expect {| map: 'a 'b. ('b -> 'a) -> 'b list -> 'a list |}]

let%expect_test _ =
  run
    {|
    let rev l =
      let rec helper acc = fun x -> match x with
        | [] -> acc
        | hd :: tl -> helper (hd :: acc) tl
      in
      helper [] l
    |} ;
  [%expect {| rev: 'a. 'a list -> 'a list |}]

let%expect_test _ =
  run
    {|
    let rec list_iter f = fun x -> match x with
      | [] -> ()
      | hd :: tl -> let () = f hd in list_iter f tl
    |} ;
  [%expect {| list_iter: 'a. ('a -> unit) -> 'a list -> unit |}]

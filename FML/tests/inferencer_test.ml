(** Copyright 2024-2025, Dmitry Pilyuk, Aleksandr Rozhkov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Fml_lib.Inferencer
open Fml_lib.Inf_pprint
open Fml_lib.Parser

let inference_program ast =
  let typ =
    match run_program_inferencer ast with
    | Ok (env, names_list) -> print_program_type env names_list
    | Error e -> print_inferencer_error e
  in
  typ
;;

let inference program =
  let ast = parse program in
  match ast with
  | Ok ast -> inference_program ast
  | Error err -> Format.printf "%s\n" err
;;

let%expect_test _ =
  inference {| let n = true|};
  [%expect {| val n : bool |}]
;;

let%expect_test _ =
  inference {| let (h::tl) = [1;2;3]|};
  [%expect
    {|
    val h : int
    val tl : int list |}]
;;

let%expect_test _ =
  inference {| let f a b = a + b|};
  [%expect {| val f : int -> int -> int |}]
;;

let%expect_test _ =
  inference
    {| let id x = x;;
let fst (a, _) = a;;
let snd (_, a) = a;; |};
  [%expect
    {|
    val id : 'a -> 'a
    val fst : 'a * 'b -> 'a
    val snd : 'a * 'b -> 'b |}]
;;

let%expect_test _ =
  inference
    {| let x = 5 and y = 6;;
let res = x + y;;|};
  [%expect
    {|
    val y : int
    val x : int
    val res : int |}]
;;

let%expect_test _ =
  inference {| let f g x = (g x : int);; |};
  [%expect
    {|
    val f : ('a -> int) -> 'a -> int |}]
;;

let%expect_test _ =
  inference {| let h::tl = [1;2;3];; |};
  [%expect
    {|
    val h : int
    val tl : int list |}]
;;

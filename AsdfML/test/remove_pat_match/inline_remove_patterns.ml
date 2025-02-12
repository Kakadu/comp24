(** Copyright 2024, Artem Khelmianov *)

(** SPDX-License-Identifier: LGPL-2.1 *)


open Base
open Lib

let test code =
  let open Format in
  match Parser.parse_program code with
  | Error e -> print_endline e
  | Ok ast ->
    (match Inferencer.inference_program ast with
     | Error e -> printf "%a" Pp_typing.pp_error e
     | Ok ast ->
       let ast = Remove_patterns.remove_patterns ast in
       printf
         "\n%s"
         (ast |> List.map ~f:(asprintf "%a" Tast.pp_tdefinition) |> String.concat))
;;

let%expect_test _ =
  test
    {|
    let const _ = 42 
  |};
  [%expect {| let const = (fun _ -> 42) |}]
;;

let%expect_test _ =
  test
    {|
    let tuple_sum (a, b) = a + b
  |};
  [%expect
    {|
    let tuple_sum = (fun `arg_0 -> match `arg_0 with
    | (a, b) -> (( + ) a b))
    |}]
;;

let%expect_test _ =
  test
    {|
    let _ = 
      let tuple_sum = fun (a, b) -> a + b in 
      tuple_sum (1, 2)
  |};
  [%expect
    {|
    let _ = let tuple_sum = (fun `arg_0 -> match `arg_0 with
    | (a, b) -> (( + ) a b))
     in (tuple_sum (1, 2))
    |}]
;;

let%expect_test _ =
  test
    {|
    let list_sum [a;b] = a + b 
  |};
  [%expect
    {|
    let list_sum = (fun `arg_0 -> match `arg_0 with
    | [a; b] -> (( + ) a b))
    |}]
;;

let%expect_test _ =
  test
    {|
    let _ = 
      let (x, y) = (1, 2) in 
      x + y
  |};
  [%expect
    {|
    let _ = match (1, 2) with
    | (x, y) -> (( + ) x y)
    |}]
;;

let%expect_test _ =
  test
    {|
    let _ = 
      let v = (1, 2) in 
      let (x, y) = v in
      x + y
  |};
  [%expect
    {|
    let _ = let v = (1, 2)
     in match v with
    | (x, y) -> (( + ) x y)
    |}]
;;

let%expect_test _ =
  test
    {|
    let normalize vec = 
      let helper = (fun (x, y, z) -> x) in 
      helper vec
  |};
  [%expect
    {|
    let normalize = (fun vec -> let helper = (fun `arg_0 -> match `arg_0 with
    | (x, y, z) -> x)
     in (helper vec))
    |}]
;;

let%expect_test _ =
  test
    {|
    let (a, b, c) = (1, 2, 3)
  |};
  [%expect
    {|
    let `temp_tuple = (1, 2, 3)
    let a = (`get_tuple_field `temp_tuple 0)
    let b = (`get_tuple_field `temp_tuple 1)
    let c = (`get_tuple_field `temp_tuple 2)
    |}]
;;

let%expect_test _ =
  test
    {|
    let [a; b; c] = [1; 2; 3; 4]
  |};
  [%expect
    {|
    let `temp_list = match [1; 2; 3; 4] with
    | [a; b; c] -> [1; 2; 3; 4]
    let a = (`list_field `temp_list 0)
    let b = (`list_field `temp_list 1)
    let c = (`list_field `temp_list 2)
    |}]
;;

let%expect_test _ =
  test
    {|
    let a::b = [1; 2; 3; 4]
  |};
  [%expect
    {|
    let `temp_list = [1; 2; 3; 4]
    let a = (`list_hd `temp_list)
    let b = (`list_tl `temp_list)
    |}]
;;

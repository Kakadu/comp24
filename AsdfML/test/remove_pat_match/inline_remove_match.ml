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
       let ast = Remove_match.remove_match ast in
       printf "\n%a" Tast.pp_tprogram ast)
;;

let%expect_test _ =
  test {|
    let const _ = 42 
  |};
  [%expect {| let const = (fun _ -> 42) |}]
;;

let%expect_test _ =
  test {|
    let tuple_sum (a, b) = a + b
  |};
  [%expect
    {|
    let tuple_sum = (fun `arg_0 ->
       let `tuple_0 = `arg_0 in
       let a = (`get_tuple_field `tuple_0 0) in
       let b = (`get_tuple_field `tuple_0 1) in
       (( + ) a b))
    |}]
;;

let%expect_test _ =
  test {|
    let f x = match x with 
    | (a, (b, c)) -> a + b + c
    | _ -> 0
  |};
  [%expect
    {|
    let f = (fun x ->
       if (( && ) (( = ) (`tuple_len x) 2) (( = ) (`tuple_len (`get_tuple_field x 1)) 2))
       then
         let `tuple_0 = x in
         let a = (`get_tuple_field `tuple_0 0) in
         let `tuple_1 = (`get_tuple_field `tuple_0 1) in
         let b = (`get_tuple_field `tuple_1 0) in
         let c = (`get_tuple_field `tuple_1 1) in
         (( + ) (( + ) a b) c)
       else 0)
    |}]
;;

let%expect_test _ =
  test
    {|
    let f x = match x with
    | [[a; b]; [c; d]] -> a + b + c + d
    | _ -> 0
  |};
  [%expect
    {|
    let f = (fun x ->
       if (( && ) (( && ) (( = ) (`list_len x) 2) (( = ) (`list_len (`list_field x 0)) 2)) (( = ) (`list_len (`list_field x 1)) 2))
       then
         let `list_0 = x in
         let `list_2 = (`list_field `list_0 0) in
         let a = (`list_field `list_2 0) in
         let b = (`list_field `list_2 1) in
         let `list_1 = (`list_field `list_0 1) in
         let c = (`list_field `list_1 0) in
         let d = (`list_field `list_1 1) in
         (( + ) (( + ) (( + ) a b) c) d)
       else 0)
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
    let _ =
       let tuple_sum = (fun `arg_0 ->
         let `tuple_0 = `arg_0 in
         let a = (`get_tuple_field `tuple_0 0) in
         let b = (`get_tuple_field `tuple_0 1) in
         (( + ) a b)) in
       (tuple_sum (1, 2))
    |}]
;;

let%expect_test _ =
  test {|
    let _ = 
      let (x, y) = (1, 2) in 
      x + y
  |};
  [%expect
    {|
    let _ =
       let `tuple_0 = (1, 2) in
       let x = (`get_tuple_field `tuple_0 0) in
       let y = (`get_tuple_field `tuple_0 1) in
       (( + ) x y)
    |}]
;;

let%expect_test _ =
  test
    {|
    let rec map f list = match list with
      | hd::tl -> f hd :: map f tl
      | _ -> []
    |};
  [%expect
    {|
    let rec map = (fun f list ->
       if (not (`list_is_empty list))
       then
         let hd = (`list_hd list) in
         let tl = (`list_tl list) in
         (( :: ) (f hd) (map f tl))
       else [])
    |}]
;;

let%expect_test _ =
  test
    {|
    let rec map = fun f -> fun (list: int list) -> match list with
    | [] -> []
    | hd::tl -> (f hd) :: (map f tl) 
  |};
  [%expect
    {|
    let rec map = (fun f -> (fun list ->
       if (`list_is_empty list)
       then []
       else
         let hd = (`list_hd list) in
         let tl = (`list_tl list) in
         (( :: ) (f hd) (map f tl))))
    |}]
;;

let%expect_test _ =
  test
    {|
    let list_mul list = 
      let rec helper acc list = match list with
        | [] -> acc
        | 0 :: _ -> 0
        | hd :: tl -> helper (hd * acc) tl
      in
      helper 1 list
  |};
  [%expect
    {|
    let list_mul = (fun list ->
       let rec helper = (fun acc list ->
         if (`list_is_empty list)
         then acc
         else
           if (( && ) (not (`list_is_empty list)) (( = ) (`list_hd list) 0))
           then 0
           else
             let hd = (`list_hd list) in
             let tl = (`list_tl list) in
             (helper (( * ) hd acc) tl)) in
       (helper 1 list))
    |}]
;;

let%expect_test _ =
  test
    {|
    let pow x n =
      let rec helper acc n =
        match n with
        | 0 -> acc
        | n -> helper (acc * x) (n - 1)
      in
      helper 1 n
  |};
  [%expect
    {|
    let pow = (fun x n ->
       let rec helper = (fun acc n ->
         if (( = ) n 0)
         then acc
         else let n = n in
           (helper (( * ) acc x) (( - ) n 1))) in
       (helper 1 n))
    |}]
;;

let%expect_test _ =
  test
    {|
    let cross (x1, y1, z1) (x2, y2, z2) =
      let x = (y1 * z2) - (z1 * y2) in
      let y = (z1 * x2) - (x1 * z2) in
      let z = (x1 * y2) - (y1 * x2) in
      (x, y, z)
  |};
  [%expect
    {|
    let cross = (fun `arg_0 `arg_1 ->
       let `tuple_1 = `arg_1 in
       let x2 = (`get_tuple_field `tuple_1 0) in
       let y2 = (`get_tuple_field `tuple_1 1) in
       let z2 = (`get_tuple_field `tuple_1 2) in
       let `tuple_0 = `arg_0 in
       let x1 = (`get_tuple_field `tuple_0 0) in
       let y1 = (`get_tuple_field `tuple_0 1) in
       let z1 = (`get_tuple_field `tuple_0 2) in
       let x = (( - ) (( * ) y1 z2) (( * ) z1 y2)) in
       let y = (( - ) (( * ) z1 x2) (( * ) x1 z2)) in
       let z = (( - ) (( * ) x1 y2) (( * ) y1 x2)) in
       (x, y, z))
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
  [%expect {|
    let sum = (fun tuples ->
       let rec helper = (fun acc tuples ->
         if (`list_is_empty tuples)
         then acc
         else
           let `tuple_1 = (`list_hd tuples) in
           let a = (`get_tuple_field `tuple_1 0) in
           let b = (`get_tuple_field `tuple_1 1) in
           let c = (`get_tuple_field `tuple_1 2) in
           let tl = (`list_tl tuples) in
           let `tuple_0 = acc in
           let x = (`get_tuple_field `tuple_0 0) in
           let y = (`get_tuple_field `tuple_0 1) in
           let z = (`get_tuple_field `tuple_0 2) in
           (helper ((( + ) a x), (( + ) b y), (( + ) c z)) tl)) in
       (helper (0, 0, 0) tuples))
    |}]
;;

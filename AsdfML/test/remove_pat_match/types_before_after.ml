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
       let ast' = Remove_patterns.remove_patterns ast in
       let ast' = Remove_match.remove_match ast' in
       let toplevel_ty = List.map ~f:(function Tast.TDLet (ty, _, pat, _) -> pat, ty) in
       let ast_ty = toplevel_ty ast in
       let ast_ty' = toplevel_ty ast' in
       (match List.zip ast_ty ast_ty' with
        | List.Or_unequal_lengths.Ok zip ->
          if List.for_all zip ~f:(fun ((lpat, lty), (rpat, rty)) ->
               Ast.equal_pattern lpat rpat && Types.equal_ty lty rty)
          then printf "Ok"
          else (
            let to_str x =
              List.map x ~f:(fun (pattern, ty) ->
                asprintf "%a: %a" Pp_ast.pp_pattern pattern Pp_typing.pp_ty ty)
              |> String.concat ~sep:"\n"
            in
            let before = to_str ast_ty in
            let after = to_str ast_ty' in
            printf "Before: %s\nAfter: %s\n" before after)
        | List.Or_unequal_lengths.Unequal_lengths _ -> printf "Unequal lengths"))
;;

let%expect_test _ =
  test {|
    let const _ = 42 
  |};
  [%expect {| Ok |}]
;;

let%expect_test _ =
  test {|
    let tuple_sum (a, b) = a + b
  |};
  [%expect {| Ok |}]
;;

let%expect_test _ =
  test {|
    let f x = match x with 
    | (a, (b, c)) -> a + b + c
    | _ -> 0
  |};
  [%expect {| Ok |}]
;;

let%expect_test _ =
  test
    {|
    let f x = match x with
    | [[a; b]; [c; d]] -> a + b + c + d
    | _ -> 0
  |};
  [%expect {| Ok |}]
;;

let%expect_test _ =
  test {|
      let tuple_sum = fun (a, b) -> a + b 
  |};
  [%expect {| Ok |}]
;;

let%expect_test _ =
  test
    {|
    let rec map f list = match list with
      | hd::tl -> f hd :: map f tl
      | _ -> []
    |};
  [%expect {| Ok |}]
;;

let%expect_test _ =
  test
    {|
    let rec map = fun f -> fun (list: int list) -> match list with
    | [] -> []
    | hd::tl -> (f hd) :: (map f tl) 
  |};
  [%expect {| Ok |}]
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
  [%expect {| Ok |}]
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
  [%expect {| Ok |}]
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
  [%expect {| Ok |}]
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
  [%expect {| Ok |}]
;;

let%expect_test _ =
  test
    {|
    let len list = 
      let rec helper acc list = match list with 
        | [] -> 0
        | [x; y] -> 2
        | a :: b :: tl -> helper (acc + 2) tl
      in
      helper 0 list
  |};
  [%expect {| Ok |}]
;;

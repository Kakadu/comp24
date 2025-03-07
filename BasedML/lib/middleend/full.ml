(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

let unpack lst =
  let rec help lst = function
    | Ok acc ->
      (match lst with
       | x :: tl ->
         (match x with
          | Error _ as err -> err
          | Ok x -> help tl (Ok (x :: acc)))
       | [] -> Ok acc)
    | Error _ as err -> err
  in
  let res = help lst (Ok []) in
  match res with
  | Ok rev_lst -> Ok (List.rev rev_lst)
  | x -> x
;;

let ( let@ ) x f =
  match x with
  | Error err -> Error err
  | Ok v -> f v
;;

let ( let$ ) x f =
  match x with
  | _, Error err -> Error err
  | _, Ok v -> f v
;;

let middleend_transform_prog prog =
  let@ simplified = Egraphs.simplify prog in
  let$ alpha_converted = Alpha_conversion.transform simplified in
  let$ match_free =
    Match_elimination.transform
      (Lambda_lifting.lift_ast (Closure_conversion.convert_ast alpha_converted))
  in
  Anf.transform match_free |> unpack
;;

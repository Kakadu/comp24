(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

let st x f =
  match x with
  | Error err -> Error err
  | Ok v -> f v
;;

let res x f =
  match x with
  | _, Error err -> Error err
  | _, Ok v -> f v
;;

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

let middleend_transform_prog prog =
  st (Egraphs.simplify prog) (fun simplified ->
    res (Alpha_conversion.transform simplified) (fun alpha_converted ->
      let closure_converted = Closure_conversion.convert_ast alpha_converted in
      let lifted = Lambda_lifting.lift_ast closure_converted in
      res (Match_elimination.transform lifted) (fun match_free ->
        let anf = Anf.transform match_free in
        anf |> unpack)))
;;

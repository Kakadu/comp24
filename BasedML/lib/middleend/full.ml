(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

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
  let _, ast = Anf.transform_anf match_free in
  ast
;;

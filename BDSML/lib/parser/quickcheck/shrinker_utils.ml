(** Copyright 2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open QCheck.Iter

let shrink_list shrink_fun = function
  | [ x ] ->
    let* x' = shrink_fun x in
    return [ x' ]
  | [] -> return []
  | l ->
    let* l' = of_list l in
    return [ l' ]
;;

(** Copyright 2024-2025, Danil Slinchuk, Julia Kononova *)

(** SPDX-License-Identifier: MIT *)

open! Base
module Ident = Ident

type 'a list1 = 'a * 'a list
[@@deriving show {with_path= false}, eq, ord, sexp_of]
type 'a list2 = 'a * 'a * 'a list
[@@deriving show {with_path= false}, eq, ord, sexp_of]

let to_list1_exn = function
  | hd :: tl ->
      (hd, tl)
  | [] ->
      raise (Invalid_argument "empty list")

let to_list2_exn = function
  | hd1 :: hd2 :: tl ->
      (hd1, hd2, tl)
  | _ ->
      raise (Invalid_argument "empty list")

let from_list1 (hd, tl) = hd :: tl
let from_list2 (hd1, hd2, tl) = hd1 :: hd2 :: tl

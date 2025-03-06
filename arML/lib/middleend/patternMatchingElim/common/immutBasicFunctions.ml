(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Pmfast

let get_list_length lst =
  let list_length_identifier = PMFIdentifier (Id ("get_list_length")) in
  PMFApplication (list_length_identifier, lst, [])
;;

let immut_equality l r = 
  let immut_equality_ident = PMFIdentifier (Id ("equal")) in
  PMFApplication (immut_equality_ident, l, [r])
;;

let immut_less_than l r = 
  let immut_less_than_ident = PMFIdentifier (Id ("less_than")) in
  PMFApplication (immut_less_than_ident, l, [r])
;;

let immut_and l r = 
  let immut_and_ident = PMFIdentifier (Id ("logic_and")) in
  PMFApplication (immut_and_ident, l, [r])
;;

let pattern_matching_failure_flag = PMFIdentifier (Id ("pattern_matching_failure"))

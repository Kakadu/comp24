(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Pmfast

let unpack_tuple pos tup = 
  let unpack_identifier = PMFIdentifier (Id ("unpack_tuple")) in
  let pos = PMFConstant (CInt pos) in
  PMFApplication (unpack_identifier, tup, [pos])
;;

let unpack_list_head lst =
  let unpack_identifier = PMFIdentifier (Id ("unpack_list_hd")) in
  PMFApplication (unpack_identifier, lst, [])
;;

let unpack_list_tail lst =
  let unpack_identifier = PMFIdentifier (Id ("unpack_list_tl")) in
  PMFApplication (unpack_identifier, lst, [])
;;

let unpack_value v = v

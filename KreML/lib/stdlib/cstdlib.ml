(** Copyright 2024-2025, CursedML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let print_int = "print_int"
let getfield = "getfield"
let stdlib_funs = [ print_int; getfield ]

let is_stdlib_fun f =
  match List.find_opt (( = ) f) stdlib_funs with
  | Some _ -> true
  | None -> false
;;

let stdlib_funs = [ print_int; getfield ]

type stdlib_fun = ident * int (* name, arity *)

let stdlib_funs_with_arity = [ print_int, 1; getfield, 2 ]

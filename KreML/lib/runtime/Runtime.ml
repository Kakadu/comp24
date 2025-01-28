(** Copyright 2024-2025, KreML Compiler Commutnity *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

let alloc_tuple = "alloc_tuple"
let alloc_closure = "alloc_closure"
let call_closure = "call_closure"
let list_cons = "list_cons"
let list_head = "list_head"
let list_tail = "list_tail"
let partial_match = "partial_match"

let partial_match_error e =
  (* let msg = Format.asprintf "Runtime error: %s" s in *)
  Expr_app (Expr_var partial_match, e)
;;

let runtime_funs =
  [ alloc_closure; alloc_tuple; list_cons; list_head; list_tail; partial_match ]
;;

let is_runtime_fun f =
  match List.find_opt (( = ) f) runtime_funs with
  | Some _ -> true
  | None -> false
;;

let print_int = "print_int"
let getfield = "getfield"
let stdlib_funs = [ print_int; getfield ]

let is_stdlib_fun f =
  match List.find_opt (( = ) f) stdlib_funs with
  | Some _ -> true
  | None -> false
;;

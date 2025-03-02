(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Llast
open Common.StateMonad

let get_new_ll_name = Common.NameCreator.get_new_name "ll"

let ll_expression env replacement_map lifted = function
  | EConstant const -> return @@ (LEConstant const, lifted)
  | _ -> 
    let _ = replacement_map in
    let _ = env in
    return (LEEmptyList, lifted)
;;
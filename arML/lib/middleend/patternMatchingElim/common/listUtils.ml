(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree

let find_with_index lst = 
  let rec helper idx = function
    | [] -> None
    | x :: xs -> if not (List.length x = 0) then Some (idx, x) else helper (idx + 1) xs
  in
  helper 0 lst
;;

let rec get_list_constr_pattern_depth = function
  | PListConstructor (_, r) -> 1 + get_list_constr_pattern_depth r
  | _ -> 0
;;

(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Common.StateMonad
open Llast

let rec map1 f = function
  | [] -> return []
  | h :: tl -> f h >>= fun c -> map1 f tl >>= fun lst -> return (c :: lst)
;;

let rec match_of_pat_let = function
  | LLConstant c -> LLConstant c |> return
  | LLIdentifier id -> LLIdentifier id |> return
  | LLIfThenElse (guard, then_branch, else_branch) ->
    let* turned_guard = match_of_pat_let guard in
    let* turned_then_branch = match_of_pat_let then_branch in
    let* turned_else_branch = match_of_pat_let else_branch in
    LLIfThenElse (turned_guard, turned_then_branch, turned_else_branch) |> return
  | LLConstraint (llex, typ) ->
    let* turned_llexp = match_of_pat_let llex in
    LLConstraint (turned_llexp, typ) |> return
  | LLApplication (left, right) ->
    let* turned_left = match_of_pat_let left in
    let* turned_right = match_of_pat_let right in
    LLApplication (turned_left, turned_right) |> return
  | LLTuple elems ->
    let* new_elems = map1 match_of_pat_let elems in
    LLTuple new_elems |> return
  | LLLetIn (_, pat, outer, inner) ->
    let* converted_outer = match_of_pat_let outer in
    let* converted_inner = match_of_pat_let inner in
    LLMatch (converted_outer, [ pat, converted_inner ]) |> return
  | LLMatch (main_exp, cases) ->
    let* new_main_exp = match_of_pat_let main_exp in
    let rec case_helper acc = function
      | [] -> List.rev acc |> return
      | (pat, expr) :: tl ->
        let* new_expr = match_of_pat_let expr in
        case_helper ((pat, new_expr) :: acc) tl
    in
    let* new_cases = case_helper [] cases in
    LLMatch (new_main_exp, new_cases) |> return
;;

let rec eliminate_match_in_declarations acc = function
  | LLDSingleLet (flag, LLLet (main_pat, args, llexpr)) :: tl ->
    let* llexpr_with_no_lets = match_of_pat_let llexpr in
    eliminate_match_in_declarations
      (LLDSingleLet (flag, LLLet (main_pat, args, llexpr_with_no_lets)) :: acc)
      tl
  | [] -> List.rev acc |> return
  | _ -> fail "unimplemented"
;;

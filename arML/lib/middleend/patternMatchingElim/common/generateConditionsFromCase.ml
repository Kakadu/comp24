(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Pmfast
open ImmutBasicFunctions
open UnpackFunctions
open ListUtils

let rec generate_pattern_matching_restrictions expr = function
  | PConst c when c <> CUnit -> [ immut_equality expr (PMFConstant c) ]
  | PNill -> [ immut_equality (get_list_length expr) (PMFConstant (CInt 0)) ]
  | PTuple (p1, p2, ps) ->
    let patterns = p1 :: p2 :: ps in
    let get_pattern_restrictions_on_expr pos pattern =
      let unpacked_expr = unpack_tuple pos expr in
      generate_pattern_matching_restrictions unpacked_expr pattern
    in
    List.mapi get_pattern_restrictions_on_expr patterns |> List.concat
  | PListConstructor (l, r) as plst ->
    let pattern_depth = get_list_constr_pattern_depth plst in
    let length_restriction = immut_less_than (PMFConstant (CInt (pattern_depth - 1))) (get_list_length expr) in
    let head_expr = unpack_list_head expr in
    let tail_expr = unpack_list_tail expr in
    length_restriction ::
    (generate_pattern_matching_restrictions head_expr l) @
    (generate_pattern_matching_restrictions tail_expr r)
  | PTyped(p, _) -> generate_pattern_matching_restrictions expr p
  | _ -> []
;;

let generate_condition_if_needed (pat, expr) b1 b2 =
  let generate_condition restrictions =
    let rec helper = function
      | [] -> PMFConstant (CBool true)
      | [last] -> last
      | hd :: tl -> immut_and hd (helper tl)
    in
    helper restrictions
  in
  let checks = generate_pattern_matching_restrictions expr pat in
  if (checks = [])
  then b1
  else 
    let condition = generate_condition checks in
    PMFIfThenElse (condition, b1, Some b2)
;;

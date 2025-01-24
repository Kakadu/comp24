(** Copyright 2024-2025, Pavel Averin, Alexey Efremov *)

(** SPDX-License-Identifier: LGPL-2.1 *)

open Ast
open Base

(* Find unbound variables within an expression *)
let unbound_identifiers exp =
  let rec helper =
    (* Takes set of unbound identifiers and binds*)
    let rec bind_pattern pat base_set =
      let rec bind_pattern_list acc lis =
        match lis with
        | h :: tl ->
          (match h with
           | PWildCard -> bind_pattern_list acc tl
           | PCons (pat1, pat2) ->
             let new_acc = bind_pattern_list acc (pat1 :: [ pat2 ]) in
             bind_pattern_list new_acc tl
           | PIdentifier id -> bind_pattern_list (Set.remove acc id) tl
           | PTuple patterns ->
             let new_acc = bind_pattern_list acc patterns in
             bind_pattern_list new_acc tl
           | PConstant _ -> bind_pattern_list acc tl
           | PConstraint (pat, _) ->
             let new_acc = bind_pattern pat base_set in
             bind_pattern_list new_acc tl)
        | [] -> acc
      in
      match pat with
      | PWildCard | PConstant _ -> base_set
      | PCons (pat1, pat2) -> bind_pattern_list base_set (pat1 :: [ pat2 ])
      | PIdentifier x -> Set.remove base_set x
      | PTuple pats -> bind_pattern_list base_set pats
      | PConstraint (pat, _) -> bind_pattern_list base_set [ pat ]
    in
    function
    | EConstant _ -> (module String) |> Set.empty
    | EIdentifier id -> Set.add ((module String) |> Set.empty) id
    | EFunction (pat, exp) ->
      let unbound_in_fun = helper exp in
      bind_pattern pat unbound_in_fun
    | EApplication (left_exp, right_exp) ->
      let unbound_in_left = helper left_exp in
      let unbound_in_right = helper right_exp in
      Set.union unbound_in_left unbound_in_right
    | EIfThenElse (guard_expr, if_expr, else_expr) ->
      let unbound_in_guard = helper guard_expr in
      let unbound_in_if = helper if_expr in
      let unbound_in_else = helper else_expr in
      Set.union (Set.union unbound_in_guard unbound_in_if) unbound_in_else
    | ELetIn (_, pat, outer_exp, inner_exp) ->
      let rec set_of_pat = function
        | PWildCard | PConstant _ -> (module String) |> Set.empty
        | PIdentifier id -> Set.add ((module String) |> Set.empty) id
        | PCons (left_pat, right_pat) ->
          Set.union (set_of_pat left_pat) (set_of_pat right_pat)
        | PTuple pats ->
          let rec tuple_helper acc ls =
            match ls with
            | [] -> acc
            | h :: tl -> tuple_helper (Set.union acc (set_of_pat h)) tl
          in
          tuple_helper ((module String) |> Set.empty) pats
        | PConstraint (pat, _) -> set_of_pat pat
      in
      let rec collect_binds acc = function
        | EFunction (pat, next) -> collect_binds (Set.union acc (set_of_pat pat)) next
        | _ -> acc
      in
      let unbound_in_outer = helper outer_exp in
      let unbound_in_inner = helper inner_exp in
      let binds = collect_binds ((module String) |> Set.empty) outer_exp in
      let unbound_in_outer_without_pat = bind_pattern pat unbound_in_outer in
      let unbound_in_inner_without_pat = bind_pattern pat unbound_in_inner in
      let unbound_in_inner_final = Set.diff unbound_in_inner_without_pat binds in
      let unbound_in_outer_final = Set.diff unbound_in_outer_without_pat binds in
      Set.union unbound_in_outer_final unbound_in_inner_final
    | ETuple exps ->
      List.fold
        exps
        ~init:((module String) |> Set.empty)
        ~f:(fun acc h -> Set.union acc (helper h))
    | EMatch (pat, branches) ->
      let unbound_in_braches =
        List.fold
          branches
          ~init:((module String) |> Set.empty)
          ~f:(fun acc (pat, exp) -> Set.union acc (bind_pattern pat (helper exp)))
      in
      bind_pattern pat unbound_in_braches
    | EConstraint (expr, _) -> helper expr
  in
  helper exp
;;
(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let eliminate_pm_case (p, expr) (matched_branch, not_matched_branch) =
  let decls = GenerateDeclarationsFromCase.generate_declarations_from_case (p, expr) in
  let rec let_expr decls =
    match decls with
    | [] -> matched_branch
    | [ lst ] -> Pmfast.PMFLetIn (lst, matched_branch)
    | hd :: tl -> Pmfast.PMFLetIn (hd, let_expr tl)
  in
  let matched_branch = let_expr decls in
  let branches = matched_branch, not_matched_branch in
  GenerateConditionsFromCase.generate_condition_if_needed (p, expr) branches
;;

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateResultMonad
open Common.StateResultMonad.Syntax
open TypeTree
open TypeErrors

let check_unique_vars patterns =
  (* Checks that all variables in the given list of patterns are unique.
     Used to detect several bound errors in tuple patterns,
     list constructor patterns, and effects with arguments. *)
  let rec helper var_set = function
    | [] -> return var_set
    | Ast.PVar (Id v) :: rest ->
      if VarSet.mem v var_set
      then fail (Several_bounds v)
      else helper (VarSet.add v var_set) rest
    | Ast.PAny :: rest -> helper var_set rest
    | Ast.PNill :: rest -> helper var_set rest
    | Ast.PConst _ :: rest -> helper var_set rest
    | Ast.PTuple (fst, snd, other) :: rest ->
      helper var_set (fst :: snd :: other @ rest)
    | Ast.PListConstructor (hd, tl) :: rest ->
      let* var_set = helper var_set [hd] in
      helper var_set (tl :: rest)
    | Ast.PTyped (pat, _) :: rest -> helper var_set (pat :: rest)
  in
  helper VarSet.empty patterns
;;

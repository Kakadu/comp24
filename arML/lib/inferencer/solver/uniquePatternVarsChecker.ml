(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open StateResultMonad
open TypeTree
open TypeErrors

let check_unique_vars pattern =
  (* Checks that all variables in the pattern are unique.
     Used to detect severeal bound errors in tuple patterns,
     list constructor patterns, and effects with arguments. *)
  let rec helper var_set = function
    | Ast.PVar (Id v) ->
      if VarSet.mem v var_set
      then
        (* If at least one variable is found twice, we raise an error. *)
        fail (Several_bounds v)
      else return (VarSet.add v var_set)
    | Ast.PAny -> return var_set
    | Ast.PNill -> return var_set
    | Ast.PConst _ -> return var_set
    | Ast.PTuple (fst, snd, other) -> RList.fold_left (fst :: snd :: other) ~init:(return var_set) ~f:helper
    | _ -> return var_set (* !!!!!! *)
  in
  helper VarSet.empty pattern
;;

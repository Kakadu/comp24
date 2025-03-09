(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.StateMonad
open Common.StateMonad.Syntax
open Common.IdentifierStructs
open Anftree
open PatternMatchingElim.Pmfast
open ExpressionToAnf

let declaration_to_anf env = function
  | PMFDOrdinary (name, args, expr) ->
    let env = IdentifierSet.add name env in
    let* expr', env' = expression_to_anf env expr in
    return ([ ADOrdinary (name, args, expr') ], env')
  | PMFDRecursive (case, cases) ->
    let cases = case :: cases in
    let* cases', env' = 
      List.fold_right
        (fun (name, args, expr) acc ->
           let* acc, env = acc in
           let env = IdentifierSet.add name env in
           let* expr', env' = expression_to_anf env expr in
           return @@ ((name, args, expr') :: acc, env'))
        cases
        (return ([], env))
    in
    return ([ ADRecursive (List.hd cases', List.tl cases') ], env')
;;

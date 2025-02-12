(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.AbstractSyntaxTree
open Common.StateMonad
open Common.StateMonad.Syntax
open Common.IdentifierStructs
open IdentifierSearcher
open IdentifierSubstitutor

let rec transform_fun = function
  | EFun ((p1, ps1), EFun ((p2, ps2), body)) ->
    transform_fun (EFun ((p1, ps1 @ (p2 :: ps2)), body))
  | e -> e
;;
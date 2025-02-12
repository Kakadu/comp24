(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Common
open IdentifierStructs

(** 
   [closure_program env program] computes the closure of the all declarations in [program] decl list
   given the environment [env].
*)
val closure_program : 
  IdentifierSet.t -> 
  (AbstractSyntaxTree.declaration) list -> 
  ((AbstractSyntaxTree.declaration) list) StateMonad.t

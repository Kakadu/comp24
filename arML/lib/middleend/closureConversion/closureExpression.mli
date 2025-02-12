(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common
open IdentifierStructs
open Ast

(** 
   [closure_expression env fv_map expr] computes the closure of the expression [expr]
   given the environment [env] and closure mapping [fv_map].
*)
val closure_expression : 
  IdentifierSet.t -> 
  (IdentifierSet.t) IdentifierMap.t -> 
  AbstractSyntaxTree.expression -> 
  (AbstractSyntaxTree.expression) StateMonad.t

(** 
   [closure_fun add_args env fv_map expr] computes the closure of the function [expr].
   If [application_flag] is true, it adds application with new arguments around the new function.
*)
val closure_fun : 
  bool ->
  IdentifierSet.t -> 
  (IdentifierSet.t) IdentifierMap.t -> 
  AbstractSyntaxTree.expression -> 
  (AbstractSyntaxTree.expression * (AbstractSyntaxTree.identifier) IdentifierMap.t) StateMonad.t

(** Copyright 2024-2025, Danil Slinchuk, Julia Kononova *)

(** SPDX-License-Identifier: MIT *)

(** Copyright 2023-2024, Danil S, Andrei *)

(** SPDX-License-Identifier: MIT *)

open! Base
open LTypes
open Smisc

type t =
  | NotImplemented of string
  | UnificationMismatch
  | VarsMismatchOrPattern of Ident.t
  | UnificationFail of  Ty.t * Ty.t
  | UnboundVariable of Ident.t
  | OccursIn of Var.t * Ty.t
  | PatVarBoundSeveralTimes of Ident.t
  | ConstructorArityMismatch of Ident.t
  | NotVarLHSRec
  | NotAllowedRHSRec of Ast.expression
  | UnboundTypeVariable of string
  | UnboundType of Ident.t
  | TypeArityMismatch of Ident.t
[@@deriving show {with_path= false}]

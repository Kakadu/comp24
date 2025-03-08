(** Copyright 2024-2025, Danil Slinchuk, Julia Kononova *)

(** SPDX-License-Identifier: MIT *)

open! Base

val parse : string -> Ast.structure option
(** Tries to parse [string]. Returns [None] if parsing fails *)

val parse_ty_exn : string -> Ast.ty
(** Tries to parse [string]. Raises [Failure] exception if parsing fails *)

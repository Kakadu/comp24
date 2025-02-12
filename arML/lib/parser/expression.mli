(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Common

(* Main expression parser *)
val parsers : dispatch
val parse_expression : expression Angstrom.t

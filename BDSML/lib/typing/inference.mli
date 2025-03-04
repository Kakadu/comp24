(** Copyright 2024-2025, Kuarni and LeonidElkin *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Parser.Ast
open Types
open Monads

val infer_program : structure_item list -> ((string * type_val) list, string) result

(** Copyright 2024-2025, raf-nr and ksenmel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Common

(* Main declaration parser *)
val parse_declaration : dispatch -> declaration Angstrom.t

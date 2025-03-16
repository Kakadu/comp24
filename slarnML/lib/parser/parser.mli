(** Copyright 2023-2025, Ivan Shurenkov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Ast

val parser : string -> (expr list, string) result

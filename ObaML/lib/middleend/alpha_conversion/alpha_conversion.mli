(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Containers

val run_alpha_conversion
  :  Ast.structure
  -> Ast.structure * VarSSet.t

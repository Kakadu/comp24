(** Copyright 2025, tepa46, Arsene-Baitenov *)

(** SPDX-License-Identifier: LGPL-2.1-or-later *)

open Containers

val run_lambda_lifting
  :  Simple_ast.sstructure
  -> VarSSet.t
  -> Simple_ast.sstructure * VarSSet.t
